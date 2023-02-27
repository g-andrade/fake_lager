%% @copyright 2022-2023 Guilherme Andrade
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

%% @private
-module(fake_lager_pr).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_context/0,
         save_record_def/3,
         pr/3]).

-define(context, 'fake_lager_pr.context').

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-record(?context, {
    record_defs :: record_defs()
}).
-opaque context() :: #?context{}.
-export_type([context/0]).

-type record_defs() :: #{
    {atom(), arity()} => record_def()
}.

-type record_def() :: #{
    field_names := [atom()]
}.

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-elvis([{elvis_style, macro_names, disable}]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new_context() -> context().
new_context() ->
    #?context{record_defs = #{}}.

-spec save_record_def(atom(), [atom()], context()) -> context().
save_record_def(Name, FieldNames, #?context{record_defs = RecordDefs} = Context) ->
    Arity = 1 + length(FieldNames),
    DefKey = {Name, Arity},
    RecordDef = new_record_def(#{field_names => FieldNames}),
    UpdatedRecordDefs = maps:put(DefKey, RecordDef, RecordDefs),
    Context#?context{record_defs = UpdatedRecordDefs}.

-spec pr(Value, Compress, Context) -> MaybePrettyValue
    when Value :: term(),
         Compress :: boolean(),
         Context :: context(),
         MaybePrettyValue :: term().
pr(MaybeRecord, Compress, Context) ->
    try element(1, MaybeRecord) of
        RecordName ->
            Record = MaybeRecord,
            RecordArity = tuple_size(Record),
            pr_with_name_and_arity(Record, RecordName, RecordArity, Compress, Context)
    catch
        error:badarg ->
            pr_not_record(_NotRecord = MaybeRecord, Compress, Context)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec new_record_def(record_def()) -> record_def().
% Trivial identity function to get Dialyzer validation
new_record_def(RecordDef) ->
    RecordDef.

pr_not_record(List, Compress, Context)
  when is_list(List) ->
    pr_list(List, Compress, Context);
pr_not_record(Tuple, Compress, Context)
  when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    MappedList = pr_list(List, Compress, Context),
    list_to_tuple(MappedList);
pr_not_record(Map, Compress, Context)
  when is_map(Map) ->
    List = maps:to_list(Map),
    MappedList = [{pr(Key, Compress, Context),
                   pr(Value, Compress, Context)}
                  || {Key, Value} <- List],
    maps:from_list(MappedList);
pr_not_record(Value, _Compress, _Context) ->
    Value.

pr_list([Head | Tail], Compress, Context) ->
    [pr(Head, Compress, Context)
     | pr_list(Tail, Compress, Context)];
pr_list([], _Compress, _Context) ->
    [];
pr_list(ImproperListTail, Compress, Context) ->
    pr(ImproperListTail, Compress, Context).

pr_with_name_and_arity(Record, RecordName, RecordArity, Compress, Context) ->
    DefKey = {RecordName, RecordArity},

    try maps:get(DefKey, Context#?context.record_defs) of
        #{field_names := FieldNames} ->
            pr_with_field_names(Record, FieldNames, Compress, Context)
    catch
        error:{badkey, K} when K =:= DefKey ->
            pr_not_record(_NotRecord = Record, Compress, Context)
    end.

pr_with_field_names(Record, FieldNames, Compress, Context) ->
    [RecordName | FieldValues] = tuple_to_list(Record),
    KeyValuesList = pr_key_values(FieldNames, FieldValues, Compress, Context),
    Tag = list_to_atom("#" ++ atom_to_list(RecordName)),
    {Tag, maps:from_list(KeyValuesList)}.

pr_key_values([Name | NextNames], [Value | NextValues], Compress, Context) ->
    case Value of
        undefined when Compress ->
            pr_key_values(NextNames, NextValues, Compress, Context);
        _ ->
            MaybePrettyValue = pr(Value, Compress, Context),
            [{Name, MaybePrettyValue} | pr_key_values(NextNames, NextValues, Compress, Context)]
    end;
pr_key_values([], [], _Compress, _Context) ->
    [].
