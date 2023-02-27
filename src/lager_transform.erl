%% @copyright 2019-2023 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(lager_transform).

-include_lib("stdlib/include/assert.hrl").
-include("lager.hrl").

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

%%-------------------------------------------------------------------
%% Internal API Function Exports
%%-------------------------------------------------------------------

-export([get_pr_context/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(pr_context_attribute_name, '___$fake_lager.pr_context').

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(module_context, {
    name :: module(),
    file :: string() | undefined,
    sinks :: [atom(), ...],
    pr_context :: fake_lager_pr:context()
}).

-record(function_context, {
    module :: module(),
    file :: string() | undefined,
    name :: atom(),
    arity :: arity(),
    sinks :: [atom(), ...]
}).

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-elvis([
    {elvis_style, macro_names, disable},
    {elvis_style, no_throw, disable}
]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec parse_transform(term(), [tuple()]) -> term().
parse_transform(Ast, Options) ->
    _ = check_for_unsupported_options(Options),
    ExtraSinks = proplists:get_value(lager_extra_sinks, Options, []),
    Sinks = [lager | ExtraSinks],

    %write_terms("ast_before.txt", Ast),
    Module = get_module(Ast),
    File = get_file(Ast),

    InitialContext = #module_context{ name = Module, file = File, sinks = Sinks,
                                      pr_context = fake_lager_pr:new_context() },
    {MappedAst, FinalContext}
        = lists:mapfoldl(fun mapfold_ast_statement/2,
                         InitialContext, Ast),

    AstWithRecordDefs
        = insert_pr_context_attribute(MappedAst,
                                       FinalContext#module_context.pr_context),

    %write_terms("ast_after.txt", AstWithRecordDefs),
    AstWithRecordDefs.

%%-------------------------------------------------------------------
%% Internal API Function Definitions
%%-------------------------------------------------------------------

-spec get_pr_context(Module) -> Context
    when Module :: module(),
         Context :: fake_lager_pr:context().
%% @private
get_pr_context(Module) ->
    Attributes = apply(Module, module_info, [attributes]),

    case lists:keyfind(?pr_context_attribute_name, 1, Attributes) of
        {?pr_context_attribute_name, [PrContext]} ->
            PrContext;
        false ->
            throw(no_pr_context)
    end.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Metadata
%%-------------------------------------------------------------------

get_module(Ast) ->
    ModuleAttribute = lists:keyfind(module, 3, Ast),
    ?assertNotEqual(false, ModuleAttribute),

    case erl_syntax_lib:analyze_module_attribute(ModuleAttribute) of
        Module when is_atom(Module) ->
            Module;
        {Module, _Params} when is_atom(Module) ->
            Module
    end.

get_file(Ast) ->
    case lists:keyfind(file, 3, Ast) of
        false ->
            undefined;
        FileAttribute ->
            {[_ | _] = File, _Line} = erl_syntax_lib:analyze_file_attribute(FileAttribute),
            File
    end.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Tree Walking
%%-------------------------------------------------------------------

mapfold_ast_statement({function, Anno, Name, Arity, Clauses}, Context) ->
    #module_context{name = Module, file = File, sinks = Sinks} = Context,
    FunctionContext = #function_context{
        module = Module,
        file = File,
        name = Name,
        arity = Arity,
        sinks = Sinks
    },
    MappedClauses = [walk_function_statements(Clause, FunctionContext) || Clause <- Clauses],
    MappedStatement = {function, Anno, Name, Arity, MappedClauses},
    {MappedStatement, Context};
mapfold_ast_statement({attribute, _, record, {Name, Fields}} = Statement, Context) ->
    #module_context{pr_context = PrContext} = Context,
    FieldNames = record_field_names(Fields),
    UpdatedPrContext = fake_lager_pr:save_record_def(Name, FieldNames, PrContext),
    UpdatedContext = Context#module_context{pr_context = UpdatedPrContext},
    {Statement, UpdatedContext};
mapfold_ast_statement(Statement, Context) ->
    {Statement, Context}.

walk_function_statements({call, Anno,
                          {remote, RemoteAnno,
                           {atom, _ModuleAnno, Module},
                           {atom, _FunctionAnno, Function}}=InvocationClause,
                          Args},
                         Context) ->
    Arity = length(Args),
    MappedArgs = walk_function_statements(Args, Context),
    SinkModules = Context#function_context.sinks,
    case lists:member(Module, SinkModules) andalso
         lists:member(Arity, [1, 2, 3]) andalso
         (lists:member(Function, ?LEVELS) orelse
          {true_but_unsafe, lists:keyfind(Function, 1, ?LEVELS_UNSAFE)})
    of
        true
          when Function =:= none -> % replace with `ok'
            {atom, RemoteAnno, ok};
        true ->
            transform_call(Anno, Function, MappedArgs, Module, Context);
        {true_but_unsafe, {_, Level}} ->
            transform_call(Anno, Level, MappedArgs, Module, Context);
        _ ->
            {call, Anno, InvocationClause, MappedArgs}
    end;
walk_function_statements(Statement, Context) when is_tuple(Statement) ->
    % very lazy way of walking the whole thing without explicit patterning
    % of all children types
    StatementParts = tuple_to_list(Statement),
    MappedStatementParts = walk_function_statements(StatementParts, Context),
    list_to_tuple(MappedStatementParts);
walk_function_statements(Statements, Context) when is_list(Statements) ->
    [walk_function_statements(Statement, Context) || Statement <- Statements];
walk_function_statements(StatementPart, _Context) ->
    StatementPart.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Transformation of Logging Calls
%%-------------------------------------------------------------------

transform_call(Anno, Level, Args, Module, Context) ->
    TransformedArgs = transform_call_args(Anno, Args, Module, Context),
    NewInvocationClause = {remote, Anno, {atom, Anno, logger}, {atom, Anno, Level}},
    {call, Anno, NewInvocationClause, TransformedArgs}.

transform_call_args(Anno, Args, Module, Context) ->
    Metadata = logging_call_metadata(Anno, Args, Module, Context),
    case is_formatting_prepararion_presumably_expensive(Args) of
        false ->
            transform_call_args_with_immediate_formatting(Anno, Args, Metadata);
        true ->
            transform_call_args_with_lazy_formatting(Anno, Args, Metadata)
    end.

is_formatting_prepararion_presumably_expensive(Args) ->
    case Args of
        [_Fmt] ->
            is_term_evaluation_presumably_expensive(Args);
        [_Fmt, _FmtArgs] ->
            is_term_evaluation_presumably_expensive(Args);
        [_, Fmt, FmtArgs] ->
            is_term_evaluation_presumably_expensive(Fmt)
            orelse is_term_evaluation_presumably_expensive(FmtArgs)
    end.

transform_call_args_with_immediate_formatting(Anno, Args, Metadata) ->
    case Args of
        [Fmt] ->
            FmtArgs = {nil, Anno}, % empty list
            [Fmt, FmtArgs, Metadata];
        [Fmt, FmtArgs] ->
            [Fmt, FmtArgs, Metadata];
        [_, Fmt, FmtArgs] ->
            [Fmt, FmtArgs, Metadata]
    end.

transform_call_args_with_lazy_formatting(Anno, Args, Metadata) ->
    case Args of
        [Fmt] ->
            FmtArgs = {nil, Anno}, % empty list
            {MsgFun, MsgArgs} = lazy_message_fun_and_args(Anno, Fmt, FmtArgs),
            [MsgFun, MsgArgs, Metadata];
        [Fmt, FmtArgs] ->
            {MsgFun, MsgArgs} = lazy_message_fun_and_args(Anno, Fmt, FmtArgs),
            [MsgFun, MsgArgs, Metadata];
        [_, Fmt, FmtArgs] ->
            {MsgFun, MsgArgs} = lazy_message_fun_and_args(Anno, Fmt, FmtArgs),
            [MsgFun, MsgArgs, Metadata]
    end.

lazy_message_fun_and_args(Anno, Fmt, FmtArgs) ->
    Fun = lazy_message_fun(Anno, Fmt, FmtArgs),
    Args = {atom, Anno, no_args},
    {Fun, Args}.

lazy_message_fun(Anno, Fmt, FmtArgs) ->
    {'fun', Anno, % anonymous fun declaration
     {clauses,
      [{clause, Anno,
        [{atom, Anno, no_args}],       % fun arguments pattern (`no_args')
        [],                            % fun guards (none)
        [{tuple, Anno, [Fmt, FmtArgs]} % fun body (`{Fmt, FmtArgs}')
        ]}
      ]}
    }.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Logging Call Metadata
%%-------------------------------------------------------------------

logging_call_metadata(Anno, Args, Module, Context) ->
    BaseMetadata = base_call_metadata(Anno, Module, Context),
    case Args of
        [_Fmt] ->
            BaseMetadata;
        [_Fmt, _FmtArgs] ->
            BaseMetadata;
        [ExtraMetadataList, _Fmt, _FmtArgs] ->
            extended_call_metadata(Anno, BaseMetadata, ExtraMetadataList)
    end.

base_call_metadata(Anno, Module, Context) ->
    Line = erl_anno:line(Anno),
    {map, Anno,
     [% mfa => {module(), atom(), arity()}
      {map_field_assoc, Anno,
       {atom, Anno, mfa},
       {tuple, Anno,
        [{atom, Anno, Context#function_context.module},
         {atom, Anno, Context#function_context.name},
         {integer, Anno, Context#function_context.arity}
        ]}}]

     % file => string()
     ++ case Context#function_context.file of
            undefined -> [];
            File ->
                [{map_field_assoc, Anno,
                  {atom, Anno, file},
                  {string, Anno, File}}]
        end

     % line => integer()
     ++ [{map_field_assoc, Anno,
          {atom, Anno, line},
          {integer, Anno, Line}}]

     % lager_sink => atom()
     ++ case Module of
            lager -> [];
            CustomSink ->
                [{map_field_assoc, Anno,
                  {atom, Anno, lager_sink},
                  {atom, Anno, CustomSink}}]
        end
    }.

extended_call_metadata(Anno, BaseMetadata, ExtraMetadataList) ->
    case transform_metadata_list_into_map_field_associations(ExtraMetadataList) of
        {true, ExtraMetadataAssociations} ->
            {map, _, BaseMetadataAssociations} = BaseMetadata,
            {map, Anno, BaseMetadataAssociations ++ ExtraMetadataAssociations};
        false ->
            runtime_merged_extended_call_metadata(Anno, BaseMetadata, ExtraMetadataList)
    end.

transform_metadata_list_into_map_field_associations(ExtraMetadataList) ->
    transform_metadata_list_into_map_field_associations_recur(ExtraMetadataList, []).

transform_metadata_list_into_map_field_associations_recur(Clause, Acc) ->
    case Clause of
        {cons, ConsAnno, {tuple, _, [{atom, _, _}=KeyTerm, ValueTerm]}, NextClause} ->
            Assoc = {map_field_assoc, ConsAnno, KeyTerm, ValueTerm},
            UpdatedAcc = [Assoc | Acc],
            transform_metadata_list_into_map_field_associations_recur(NextClause, UpdatedAcc);

        {nil, _} ->
            Associations = lists:reverse(Acc),
            {true, Associations};

        _ ->
            false
    end.

runtime_merged_extended_call_metadata(Anno, BaseMetadata, ExtraMetadataList) ->
    % maps:merge(ExtraMetadata, BaseMetadata)
    {call, Anno,
     {remote, Anno, {atom, Anno, maps}, {atom, Anno, merge}},

     [% Map1 - maps:from_list(ExtraMetadataList)
      {call, Anno, {remote, Anno, {atom, Anno, maps},
                    {atom, Anno, from_list}}, [ExtraMetadataList]},

      % Map2 - BaseMetadata
      BaseMetadata
     ]}.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Pretty Printing of Records
%%-------------------------------------------------------------------

record_field_names(FieldsFromAst) ->
    lists:map(fun record_field_name/1, FieldsFromAst).

%% `record_field_name/1' copied from original `lager_transform',
%% licensed under Apache 2.0
record_field_name({record_field, _, {atom, _, FieldName}}) ->
    FieldName;
record_field_name({record_field, _, {atom, _, FieldName}, _Default}) ->
    FieldName;
record_field_name({typed_record_field, Field, _Type}) ->
    record_field_name(Field).

insert_pr_context_attribute([Attribute | Next], RecordDefs) ->
    case Attribute of
        {attribute, Line, module, _} = ModuleAttribute ->
            [ModuleAttribute
             , {attribute, Line, ?pr_context_attribute_name, [RecordDefs]}
             | Next];
        OtherAttribute ->
            [OtherAttribute
             | insert_pr_context_attribute(Next, RecordDefs)]
    end;
insert_pr_context_attribute([], _RecordDefs) ->
    % There's no module attribute - let the compiler handle it rather than have us crash
    [].

%%-------------------------------------------------------------------
%% Internal Function Definitions - Utiliities
%%-------------------------------------------------------------------

check_for_unsupported_options(Options) ->
    lists:foreach(
      fun ({lager_print_records_flag, true}) ->
              ok;
          ({Key, Value}) when Key =:= lager_truncation_size;
                              Key =:= lager_print_records_flag;
                              Key =:= lager_function_transforms ->
              error_logger:error_msg("[error] Unsupported option: '~s~n'", [{Key, Value}]),
              exit(normal);
          (_) ->
              ok
      end,
      Options).

% XXX: should binary construction be considered expensive?
is_term_evaluation_presumably_expensive({call, _Anno, _InvocationClause, _Args}) ->
    % any function call
    true;
is_term_evaluation_presumably_expensive({'receive', _Anno, _Patterns}) ->
    % receive pattern
    true;
is_term_evaluation_presumably_expensive({'receive', _Anno, _Patterns, _Timeout,
                                         _TimeoutHandlers}) ->
    % receive pattern (with timeouts)
    true;
is_term_evaluation_presumably_expensive({op, _Anno, _Pid, _Arg}) ->
    % unary operator (e.g. `-')
    true;
is_term_evaluation_presumably_expensive({op, _Anno, _Pid, _Arg1, _Arg2}) ->
    % binary operator (e.g. `+')
    true;
is_term_evaluation_presumably_expensive({'case', _Anno, _Expression, _Patterns}) ->
    % case block
    true;
is_term_evaluation_presumably_expensive({'if', _Anno, _Patterns}) ->
    % if block
    true;
is_term_evaluation_presumably_expensive(Term) when is_tuple(Term) ->
    TermParts = tuple_to_list(Term),
    lists:any(fun is_term_evaluation_presumably_expensive/1, TermParts);
is_term_evaluation_presumably_expensive(Term) when is_list(Term) ->
    lists:any(fun is_term_evaluation_presumably_expensive/1, Term);
is_term_evaluation_presumably_expensive(_) ->
    false.

%write_terms(FilenameSuffix, List) ->
%    {attribute, _Anno, module, Module} = lists:keyfind(module, 3, List),
%    Filename = atom_to_list(Module) ++ "." ++ FilenameSuffix,
%    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
%    Text = lists:map(Format, List),
%    file:write_file(Filename, Text).
