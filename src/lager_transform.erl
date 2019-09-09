%% @copyright 2019 Guilherme Andrade
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

-include("lager.hrl").

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(module_context, {
          name :: module(),
          file :: string() | undefined
         }).

-record(function_context, {
          module :: module(),
          file :: string() | undefined,
          name :: atom(),
          arity :: arity()
         }).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec parse_transform(term(), [tuple()]) -> term().
parse_transform(AST, Options) ->
    _ = check_for_unsupported_options(Options),

    %write_terms("ast_before.txt", AST),
    {attribute, _Line, module, Module} = lists:keyfind(module, 3, AST),
    File =
        case lists:keyfind(file, 3, AST) of
            {attribute, _Line, file, DefinedFile} ->
                DefinedFile;
            false ->
                undefined
        end,

    Context = #module_context{ name = Module, file = File },
    MappedAST = [map_ast_statement(Statement, Context) || Statement <- AST],
    %write_terms("ast_after.txt", MappedAST),
    MappedAST.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Tree Walking
%%-------------------------------------------------------------------

map_ast_statement({function, Line, Name, Arity, Clauses}, Context) ->
    #module_context{name = Module, file = File} = Context,
    FunctionContext = #function_context{ module = Module, file = File, name = Name, arity = Arity },
    MappedClauses = [walk_function_statements(Clause, FunctionContext) || Clause <- Clauses],
    {function, Line, Name, Arity, MappedClauses};
map_ast_statement(Statement, _Context) ->
    Statement.

walk_function_statements({call, Line,
                          {remote, _RemoteLine,
                           {atom, _ModuleLine, Module},
                           {atom, _FunctionLine, Function}}=InvocationClause,
                          Args},
                         Context) ->
    Arity = length(Args),
    MappedArgs = walk_function_statements(Args, Context),
    SinkModules = [lager],
    case lists:member(Module, SinkModules) andalso
         lists:member(Arity, [1,2,3]) andalso
         (lists:member(Function, ?LEVELS) orelse
          {true_but_unsafe, lists:keyfind(Function, 1, ?LEVELS_UNSAFE)})
    of
        false ->
            {call, Line, InvocationClause, MappedArgs};
        true
          when Function =/= none -> % TODO filter out `none' calls?
            transform_call(Line, Function, MappedArgs, Context);
        {true_but_unsafe, {_,Level}} ->
            transform_call(Line, Level, MappedArgs, Context)
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

transform_call(Line, Level, Args, Context) ->
    TransformedArgs = transform_call_args(Line, Args, Context),
    NewInvocationClause = {remote, Line, {atom,Line,logger}, {atom,Line,Level}},
    {call, Line, NewInvocationClause, TransformedArgs}.

transform_call_args(Line, Args, Context) ->
    Metadata = logging_call_metadata(Line, Args, Context),
    case is_formatting_prepararion_presumably_expensive(Args) of
        false ->
            transform_call_args_with_immediate_formatting(Line, Args, Metadata);
        true ->
            transform_call_args_with_lazy_formatting(Line, Args, Metadata)
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

transform_call_args_with_immediate_formatting(Line, Args, Metadata) ->
    case Args of
        [Fmt] ->
            FmtArgs = {nil,Line}, % empty list
            [Fmt, FmtArgs, Metadata];
        [Fmt, FmtArgs] ->
            [Fmt, FmtArgs, Metadata];
        [_, Fmt, FmtArgs] ->
            [Fmt, FmtArgs, Metadata]
    end.

transform_call_args_with_lazy_formatting(Line, Args, Metadata) ->
    case Args of
        [Fmt] ->
            FmtArgs = {nil,Line}, % empty list
            {MsgFun, MsgArgs} = lazy_message_fun_and_args(Line, Fmt, FmtArgs),
            [MsgFun, MsgArgs, Metadata];
        [Fmt, FmtArgs] ->
            {MsgFun, MsgArgs} = lazy_message_fun_and_args(Line, Fmt, FmtArgs),
            [MsgFun, MsgArgs, Metadata];
        [_, Fmt, FmtArgs] ->
            {MsgFun, MsgArgs} = lazy_message_fun_and_args(Line, Fmt, FmtArgs),
            [MsgFun, MsgArgs, Metadata]
    end.

lazy_message_fun_and_args(Line, Fmt, FmtArgs) ->
    Fun = lazy_message_fun(Line, Fmt, FmtArgs),
    Args = {atom, Line, no_args},
    {Fun, Args}.

lazy_message_fun(Line, Fmt, FmtArgs) ->
    {'fun', Line, % anonymous fun declaration
     {clauses,
      [{clause, Line,
        [{atom, Line, no_args}],       % fun arguments pattern (`no_args')
        [],                            % fun guards (none)
        [{tuple, Line, [Fmt, FmtArgs]} % fun body (`{Fmt, FmtArgs}')
        ]}
      ]}
    }.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Logging Call Metadata
%%-------------------------------------------------------------------

logging_call_metadata(Line, Args, Context) ->
    BaseMetadata = base_call_metadata(Line, Context),
    case Args of
        [_Fmt] ->
            BaseMetadata;
        [_Fmt, _FmtArgs] ->
            BaseMetadata;
        [ExtraMetadataList, _Fmt, _FmtArgs] ->
            extended_call_metadata(Line, BaseMetadata, ExtraMetadataList)
    end.

base_call_metadata(Line, Context) ->
    {map, Line,
     [% mfa => {module(), atom(), arity()}
      {map_field_assoc, Line,
       {atom, Line, mfa},
       {tuple, Line,
        [{atom, Line, Context#function_context.module},
         {atom, Line, Context#function_context.name},
         {integer, Line, Context#function_context.arity}
        ]}}]

     % file => string()
     ++ case Context#function_context.file of
            undefined -> [];
            File ->
                [{map_field_assoc, Line,
                  {atom, Line, file},
                  {string, Line, File}}]
        end

     % line => integer()
     ++ [{map_field_assoc, Line,
          {atom, Line, line},
          {integer, Line, Line}}]
    }.

extended_call_metadata(Line, BaseMetadata, ExtraMetadataList) ->
    case transform_metadata_list_into_map_field_associations(ExtraMetadataList) of
        {true, ExtraMetadataAssociations} ->
            {map, Line, BaseMetadataAssociations} = BaseMetadata,
            {map, Line, BaseMetadataAssociations ++ ExtraMetadataAssociations};
        false ->
            runtime_merged_extended_call_metadata(Line, BaseMetadata, ExtraMetadataList)
    end.

transform_metadata_list_into_map_field_associations(ExtraMetadataList) ->
    transform_metadata_list_into_map_field_associations_recur(ExtraMetadataList, []).

transform_metadata_list_into_map_field_associations_recur(Clause, Acc) ->
    case Clause of
        {cons, ConsLine, {tuple,_,[{atom,_,_}=KeyTerm,ValueTerm]}, NextClause} ->
            Assoc = {map_field_assoc, ConsLine, KeyTerm, ValueTerm},
            UpdatedAcc = [Assoc | Acc],
            transform_metadata_list_into_map_field_associations_recur(NextClause, UpdatedAcc);

        {nil, _} ->
            Associations = lists:reverse(Acc),
            {true, Associations};

        _ ->
            false
    end.

runtime_merged_extended_call_metadata(Line, BaseMetadata, ExtraMetadataList) ->
    % maps:merge(ExtraMetadata, BaseMetadata)
    {call, Line,
     {remote, Line, {atom,Line,maps}, {atom,Line,merge}},

     [% Map1 - maps:from_list(ExtraMetadataList)
      {call, Line, {remote, Line, {atom,Line,maps}, {atom,Line,from_list}}, [ExtraMetadataList]},

      % Map2 - BaseMetadata
      BaseMetadata
     ]}.

%%-------------------------------------------------------------------
%% Internal Function Definitions - Utiliities
%%-------------------------------------------------------------------

check_for_unsupported_options(Options) ->
    lists:foreach(
      fun ({Key,_Value}) when Key =:= lager_truncation_size;
                              Key =:= lager_print_records_flag;
                              Key =:= lager_extra_sinks;
                              Key =:= lager_function_transforms ->
              io:format("Unsupported option: '~s'", [Key]),
              exit(normal);
          (_) ->
              ok
      end,
      Options).

% XXX: should binary construction be considered expensive?
is_term_evaluation_presumably_expensive({call, _Line, _InvocationClause, _Args}) ->
    % any function call
    true;
is_term_evaluation_presumably_expensive({'receive', _Line, _Patterns}) ->
    % receive pattern
    true;
is_term_evaluation_presumably_expensive({'receive', _Line, _Patterns, _Timeout, _TimeoutHandlers}) ->
    % receive pattern (with timeouts)
    true;
is_term_evaluation_presumably_expensive({op, _Line, _Pid, _Arg}) ->
    % unary operator (e.g. `-')
    true;
is_term_evaluation_presumably_expensive({op, _Line, _Pid, _Arg1, _Arg2}) ->
    % binary operator (e.g. `+')
    true;
is_term_evaluation_presumably_expensive({'case', _Line, _Expression, _Patterns}) ->
    % case block
    true;
is_term_evaluation_presumably_expensive({'if', _Line, _Patterns}) ->
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
%    {attribute, _Line, module, Module} = lists:keyfind(module, 3, List),
%    Filename = atom_to_list(Module) ++ "." ++ FilenameSuffix,
%    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
%    Text = lists:map(Format, List),
%    file:write_file(Filename, Text).
