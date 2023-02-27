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

-module(lager).

-include("lager.hrl").

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [clear_all_traces/0,
    clear_trace_by_destination/1,
    dispatch_log/5,
    dispatch_log/7,
    dispatch_log/9,
    do_log/10,
    do_log/9,
    do_log_unsafe/10,
    get_loglevel/1,
    get_loglevel/2,
    get_loglevels/1,
    install_trace/2,
    install_trace/3,
    list_all_sinks/0,
    log/3,
    log/4,
    log/5,
    log_unsafe/4,
    md/0,
    md/1,
    posix_error/1,
    pr/2,
    pr/3,
    pr_stacktrace/1,
    pr_stacktrace/2,
    remove_trace/1,
    rotate_all/0,
    rotate_handler/1,
    rotate_handler/2,
    rotate_sink/1,
    safe_format/3,
    safe_format_chop/3,
    set_loghwm/2,
    set_loghwm/3,
    set_loghwm/4,
    set_loglevel/2,
    set_loglevel/3,
    set_loglevel/4,
    start/0,
    status/0,
    stop_trace/1,
    stop_trace/3,
    trace/2,
    trace/3,
    trace_console/1,
    trace_console/2,
    trace_file/2,
    trace_file/3,
    trace_file/4,
    trace_func/3,
    trace_state/3,
    unsafe_format/2,
    update_loglevel_config/1
   ]).

-ignore_xref(
   [clear_all_traces/0,
    clear_trace_by_destination/1,
    dispatch_log/5,
    dispatch_log/7,
    dispatch_log/9,
    do_log/9,
    do_log/10,
    do_log_unsafe/10,
    get_loglevel/1,
    get_loglevel/2,
    get_loglevels/1,
    install_trace/2,
    install_trace/3,
    list_all_sinks/0,
    log/3,
    log/4,
    log/5,
    log_unsafe/4,
    md/0,
    md/1,
    posix_error/1,
    pr/2,
    pr/3,
    pr_stacktrace/1,
    pr_stacktrace/2,
    remove_trace/1,
    rotate_all/0,
    rotate_handler/1,
    rotate_handler/2,
    rotate_sink/1,
    safe_format/3,
    safe_format_chop/3,
    set_loghwm/2,
    set_loghwm/3,
    set_loghwm/4,
    set_loglevel/2,
    set_loglevel/3,
    set_loglevel/4,
    start/0,
    status/0,
    stop_trace/1,
    stop_trace/3,
    trace/2,
    trace/3,
    trace_console/1,
    trace_console/2,
    trace_file/2,
    trace_file/3,
    trace_file/4,
    trace_func/3,
    trace_state/3,
    unsafe_format/2,
    update_loglevel_config/1
   ]).

%%-------------------------------------------------------------------
%% Static Check Tweaks
%%-------------------------------------------------------------------

-hank([
    {unnecessary_function_arguments, [
        {clear_trace_by_destination, 1},
        {dispatch_log, 7},
        {dispatch_log, 9},
        {do_log, 10},
        {do_log_unsafe, 10},
        {get_loglevel, 1},
        {get_loglevel, 2},
        {get_loglevels, 1},
        {install_trace, 2},
        {install_trace, 3},
        {posix_error, 1},
        {pr_stacktrace, 1},
        {pr_stacktrace, 2},
        {remove_trace, 1},
        {rotate_handler, 1},
        {rotate_handler, 2},
        {rotate_sink, 1},
        {safe_format, 3},
        {safe_format_chop, 3},
        {set_loghwm, 2},
        {set_loghwm, 3},
        {set_loghwm, 4},
        {set_loglevel, 2},
        {set_loglevel, 3},
        {set_loglevel, 4},
        {stop_trace, 3},
        {trace, 2},
        {trace, 3},
        {trace_console, 1},
        {trace_console, 2},
        {trace_file, 2},
        {trace_file, 3},
        {trace_file, 4},
        {trace_func, 3},
        {trace_state, 3},
        {unsafe_format, 2},
        {update_loglevel_config, 1}
    ]}
]).

%%-------------------------------------------------------------------
%% Macro Definitions
%%-------------------------------------------------------------------

-define(DEFAULT_PR_COMPRESS, false).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-type log_level() :: none | logger:level().
-export_type([log_level/0]).

-type log_level_number() :: 0..7.
-export_type([log_level_number/0]).

-type pr_opt() :: boolean_opt(compress).
-export_type([pr_opt/0]).

-type boolean_opt(Name) :: Name | {Name, boolean()}.
-export_type([boolean_opt/1]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec clear_all_traces() -> no_return().
%% @private
clear_all_traces() ->
    error(notsup).

-spec clear_trace_by_destination(term()) -> no_return().
%% @private
clear_trace_by_destination(_Id) ->
    error(notsup).

-spec dispatch_log(Severity, MetadataList, Format, Args, TruncSize) -> ok
        when Severity :: log_level(),
             MetadataList :: [{atom(),term()}],
             Format :: string(),
             Args :: list(),
             TruncSize :: non_neg_integer().
%% @doc Backwards compatible with beams compiled with lager `2.x'
%% @deprecated
dispatch_log(Severity, MetadataList, Format, Args, TruncSize) ->
    dispatch_log(?DEFAULT_SINK, Severity, MetadataList, Format, Args, TruncSize, safe).

-spec dispatch_log(Sink, Severity, MetadataList, Format, Args, TruncSize, Safety) -> ok | Error
        when Sink :: atom(),
             Severity :: log_level(),
             MetadataList :: [{atom(),term()}],
             Format :: string(),
             Args :: list(),
             TruncSize :: non_neg_integer(),
             Safety :: safe | unsafe,
             Error :: {error, Reason},
             Reason :: {bad_sink, Sink}.
dispatch_log(Sink, Severity, MetadataList, Format, Args, TruncSize, _Safety)
  when is_atom(Severity) ->
    SeverityAsInt = -1,
    LevelThreshold = -1,
    Traces = [],
    SinkPid = whereis(Sink),
    do_log(Severity, MetadataList, Format, Args, TruncSize, SeverityAsInt,
           LevelThreshold, Traces, Sink, SinkPid).

-spec dispatch_log(Severity, Module, Function, Line, Pid, MetadataList, Format, Args, TruncSize) -> ok
        when Severity :: log_level(),
             Module :: module(),
             Function :: atom(),
             Line :: pos_integer(),
             Pid :: pid(),
             MetadataList :: [{atom(),term()}],
             Format :: string(),
             Args :: list(),
             TruncSize :: non_neg_integer().
%% @doc Backwards compatible with beams compiled with lager `1.x'
%% @deprecated
dispatch_log(Severity, _Module, _Function, _Line, _Pid, MetadataList, Format, Args, TruncSize) ->
    dispatch_log(Severity, MetadataList, Format, Args, TruncSize).

-spec do_log(Severity, MetadataList, Format, Args, TruncSize, SeverityAsInt,
             LevelThreshold, TraceFilters, SinkPid) -> ok
        when Severity :: log_level(),
             MetadataList :: [{atom(),term()}],
             Format :: string(),
             Args :: list(),
             TruncSize :: non_neg_integer(),
             SeverityAsInt :: integer(),
             LevelThreshold :: integer(),
             TraceFilters :: term(),
             SinkPid :: pid().
%% @doc Backwards compatible with beams compiled with lager `2.x'
%% @private
%% @deprecated
do_log(Severity, MetadataList, Format, Args, TruncSize, SeverityAsInt, LevelThreshold, TraceFilters, SinkPid) ->
    do_log(Severity, MetadataList, Format, Args, TruncSize, SeverityAsInt,
           LevelThreshold, TraceFilters, ?DEFAULT_SINK, SinkPid).

-spec do_log(Severity, MetadataList, Format, Args, TruncSize, SeverityAsInt,
             LevelThreshold, TraceFilters, Sink, SinkPid) -> ok | Error
        when Severity :: log_level(),
             MetadataList :: [{atom(),term()}],
             Format :: string(),
             Args :: list(),
             TruncSize :: non_neg_integer(),
             SeverityAsInt :: integer(),
             LevelThreshold :: integer(),
             TraceFilters :: term(),
             Sink :: atom(),
             SinkPid :: pid(),
             Error :: {error, Reason},
             Reason :: {bad_sink, Sink}.
%% @private Should only be called externally from code generated from the parse transform
do_log(Severity, MetadataList, Format, Args,
       _Size, _SeverityAsInt, _LevelThreshold, _TraceFilters,
       Sink, _SinkPid)
  when is_atom(Severity) ->
    redirect_to_logger(Severity, MetadataList, Format, Args, Sink).

-spec do_log_unsafe(Severity, MetadataList, Format, Args, TruncSize, SeverityAsInt,
                    LevelThreshold, TraceFilters, Sink, SinkPid) -> ok | Error
        when Severity :: log_level(),
             MetadataList :: [{atom(),term()}],
             Format :: string(),
             Args :: list(),
             TruncSize :: non_neg_integer(),
             SeverityAsInt :: integer(),
             LevelThreshold :: integer(),
             TraceFilters :: term(),
             Sink :: atom(),
             SinkPid :: pid(),
             Error :: {error, Reason},
             Reason :: {bad_sink, Sink}.
%% @private Should only be called externally from code generated from the parse transform
%% Specifically, it would be level ++ `_unsafe' as in `info_unsafe'.
do_log_unsafe(Severity, MetadataList, Format, Args,
              _Size, _SeverityAsInt, _LevelThreshold, _TraceFilters,
              Sink, _SinkPid)
  when is_atom(Severity) ->
    redirect_to_logger(Severity, MetadataList, Format, Args, Sink).

-spec get_loglevel(term()) -> no_return().
%% @private
get_loglevel(_Handler) ->
    error(notsup).

-spec get_loglevel(term(), term()) -> no_return().
%% @private
get_loglevel(_Sink, _Handler) ->
    error(notsup).

-spec get_loglevels(term()) -> no_return().
%% @private
get_loglevels(_Sink) ->
    error(notsup).

-spec install_trace(term(), term()) -> no_return().
%% @private
install_trace(_Pid, _Level) ->
    error(notsup).

-spec install_trace(term(), term(), term()) -> no_return().
%% @private
install_trace(_Pid, _Level, _Options) ->
    error(notsup).

-spec list_all_sinks() -> no_return().
%% @private
list_all_sinks() ->
    error(notsup).

-spec log(Level, Process | MetadataList, Message) -> ok
        when Level :: log_level(),
             Process :: pid() | atom(),
             MetadataList :: [{atom(),term()}],
             Message :: string().
%% @doc Manually log a message into lager without using the parse transform.
log(Level, Process, Message)
  when is_pid(Process); is_atom(Process) ->
    dispatch_log(Level, [{pid,Process}], Message, [], ?DEFAULT_TRUNCATION);
log(Level, MetadataList, Message)
  when is_list(MetadataList) ->
    dispatch_log(Level, MetadataList, Message, [], ?DEFAULT_TRUNCATION).

-spec log(Level, Process | MetadataList, Message, Args) -> ok
        when Level :: log_level(),
             Process :: pid() | atom(),
             MetadataList :: [{atom(),term()}],
             Message :: string(),
             Args :: list().
%% @doc Manually log a message into lager without using the parse transform.
log(Level, Process, Format, Args)
  when is_pid(Process); is_atom(Process) ->
    dispatch_log(Level, [{pid,Process}], Format, Args, ?DEFAULT_TRUNCATION);
log(Level, MetadataList, Format, Args)
  when is_list(MetadataList) ->
    dispatch_log(Level, MetadataList, Format, Args, ?DEFAULT_TRUNCATION).

-spec log(Sink, Level, Process | MetadataList, Message, Args) -> ok | Error
        when Sink :: atom(),
             Level :: log_level(),
             Process :: pid() | atom(),
             MetadataList :: [{atom(),term()}],
             Message :: string(),
             Args :: list(),
             Error :: {error, Reason},
             Reason :: {bad_sink, Sink}.
%% @doc Manually log a message into lager without using the parse transform.
log(Sink, Level, Pid, Format, Args)
  when is_pid(Pid); is_atom(Pid) ->
    dispatch_log(Sink, Level, [{pid,Pid}], Format, Args, ?DEFAULT_TRUNCATION, safe);
log(Sink, Level, MetadataList, Format, Args) when is_list(MetadataList) ->
    dispatch_log(Sink, Level, MetadataList, Format, Args, ?DEFAULT_TRUNCATION, safe).

-spec log_unsafe(Level, MetadataList, Message, Args) -> ok
        when Level :: log_level(),
             MetadataList :: [{atom(),term()}],
             Message :: string(),
             Args :: list().
log_unsafe(Level, MetadataList, Format, Args)
  when is_list(MetadataList) ->
    dispatch_log(?DEFAULT_SINK, Level, MetadataList, Format, Args, ?DEFAULT_TRUNCATION, unsafe).

%% @doc Get lager metadata for current process
-spec md() -> [{atom(), any()}].
md() ->
    case logger:get_process_metadata() of
        undefined -> [];
        Metadata ->
            maps:to_list(Metadata)
    end.

%% @doc Set lager metadata for current process.
%% Will badarg if you don't supply a list of {key, value} tuples keyed by atoms.
-spec md([{atom(), any()},...]) -> ok.
md(MetadataList)
  when length(MetadataList) >= 0 ->
    Metadata = logger_metadata(MetadataList),
    logger:set_process_metadata(Metadata);
md(_) ->
    error(badarg).

-spec posix_error(term()) -> no_return().
%% @private
posix_error(_Error) ->
    error(notsup).

-spec pr(Value, Module) -> MaybePrettyValue
    when Value :: Record | [Record] | NonRecord,
         Record :: tuple(),
         NonRecord :: term(),
         Module :: module(),
         MaybePrettyValue :: PrettyRecord | [PrettyRecord] | MaybePrettyNonRecord,
         PrettyRecord :: {RecordName, PrettyRecordFields},
         RecordName :: atom(),
         PrettyRecordFields :: #{RecordFieldName => MaybePrettyValue},
         RecordFieldName :: atom(),
         MaybePrettyNonRecord :: term().
%% @doc Pretty-prints records with known definitions as tagged maps
pr(Value, Module) ->
    pr(Value, Module, _Opts = []).

-spec pr(Value, Module, Opts) -> MaybePrettyValue
    when Value :: Record | [Record] | NonRecord,
         Record :: tuple(),
         NonRecord :: term(),
         Module :: module(),
         MaybePrettyValue :: PrettyRecord | [PrettyRecord] | MaybePrettyNonRecord,
         PrettyRecord :: {RecordName, PrettyRecordFields},
         RecordName :: atom(),
         PrettyRecordFields :: #{RecordFieldName => MaybePrettyValue},
         RecordFieldName :: atom(),
         MaybePrettyNonRecord :: term(),
         Opts :: [pr_opt()].
%% @doc Pretty-prints records with known definitions as tagged maps
pr(Value, Module, Opts) ->
    Compress = proplists:get_value(compress, Opts, ?DEFAULT_PR_COMPRESS),

    try lager_transform:get_pr_context(Module) of
        PrContext ->
            fake_lager_pr:pr(Value, Compress, PrContext)
    catch
        throw:no_pr_context ->
            Value
    end.

-spec pr_stacktrace(term()) -> no_return().
%% @TODO
%% @private
pr_stacktrace(_Stacktrace) ->
    error(notsup).

-spec pr_stacktrace(term(), {atom(),term()}) -> no_return().
%% @TODO
%% @private
pr_stacktrace(_Stacktrace, {_Class, _Reason}) ->
    error(notsup).

-spec remove_trace(term()) -> no_return().
%% @private
remove_trace(_Pid) ->
    error(notsup).

-spec rotate_all() -> no_return().
%% @private
rotate_all() ->
    error(notsup).

-spec rotate_handler(term()) -> no_return().
%% @private
rotate_handler(_Handler) ->
    error(notsup).

-spec rotate_handler(term(), term()) -> no_return().
%% @private
rotate_handler(_Handler, _Sink) ->
    error(notsup).

-spec rotate_sink(term()) -> no_return().
%% @private
rotate_sink(_Sink) ->
    error(notsup).

-spec safe_format(term(), term(), term()) -> no_return().
%% @private
safe_format(_Fmt, _Args, _Limit) ->
    error(notsup).

-spec safe_format_chop(term(), term(), term()) -> no_return().
%% @private
safe_format_chop(_Fmt, _Args, _Limit) ->
    error(notsup).

-spec set_loghwm(term(), term()) -> no_return().
%% @private
set_loghwm(_Handler, _Hwm) ->
    error(notsup).

-spec set_loghwm(term(), term(), term()) -> no_return().
%% @private
set_loghwm(_Sink, _Handler, _Hwm) ->
    error(notsup).

-spec set_loghwm(term(), term(), term(), term()) -> no_return().
%% @private
set_loghwm(_Sink, _Handler, _Ident, _Hwm) ->
    error(notsup).

-spec set_loglevel(term(), term()) -> no_return().
%% @private
set_loglevel(_Handler, _Level) ->
    error(notsup).

-spec set_loglevel(term(), term(), term()) -> no_return().
%% @private
set_loglevel(_Handler, _Ident, _Level) ->
    error(notsup).

-spec set_loglevel(term(), term(), term(), term()) -> no_return().
%% @private
set_loglevel(_Sink, _Handler, _Ident, _Level) ->
    error(notsup).

-spec start() -> ok | {error,{atom(),term()}}.
start() ->
    case application:ensure_all_started(lager) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec status() -> no_return().
%% @private
status() ->
    error(notsup).

-spec stop_trace({term(),term(),term()}) -> no_return().
%% @private
stop_trace({_Backend, _Filter, _Level}) ->
    error(notsup).

-spec stop_trace(term(), term(), term()) -> no_return().
%% @private
stop_trace(_Backend, _Filter, _Level) ->
    error(notsup).

-spec trace(term(), term()) -> no_return().
%% @private
trace(_Backend, _Filter) ->
    error(notsup).

-spec trace(term(), term(), term()) -> no_return().
%% @private
trace(_Backend, _Filter, _Level) ->
    error(notsup).

-spec trace_console(term()) -> no_return().
%% @private
trace_console(_Filter) ->
    error(notsup).

-spec trace_console(term(), term()) -> no_return().
%% @private
trace_console(_Filter, _Level) ->
    error(notsup).

-spec trace_file(term(), term()) -> no_return().
%% @private
trace_file(_File, _Filter) ->
    error(notsup).

-spec trace_file(term(), term(), term()) -> no_return().
%% @private
trace_file(_File, _Filter, _LevelOrOptions) ->
    error(notsup).

-spec trace_file(term(), term(), term(), term()) -> no_return().
%% @private
trace_file(_File, _Filter, _Level, _Options) ->
    error(notsup).

-spec trace_func(term(), term(), term()) -> no_return().
%% @private
trace_func(_FuncState, _Event, _ProcState) ->
    error(notsup).

-spec trace_state(term(), term(), term()) -> no_return().
%% @private
trace_state(_Pid, _Level, _Options) ->
    error(notsup).

-spec unsafe_format(term(), term()) -> no_return().
%% @private
unsafe_format(_Fmt, _Args) ->
    error(notsup).

-spec update_loglevel_config(term()) -> no_return().
%% @private
update_loglevel_config(_Sink) ->
    error(notsup).

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

logger_metadata(MetadataList) ->
    case metadata_map(MetadataList) of
        #{module := Module, function := Function} = Metadata
          when not is_map_key(mfa, Metadata) ->
            Arity = maps:get(arity, Metadata, 0),
            WithoutLagerSpecifics = maps:without([module, function, arity], Metadata),
            maps:put(mfa, {Module, Function, Arity}, WithoutLagerSpecifics);

        Metadata ->
            Metadata
    end.

metadata_map(MetadataList) ->
    lists:foldl(
      fun({Key,Value}, Acc) when is_atom(Key) ->
              Acc#{ Key => Value };
         (_, _) ->
              error(badarg)
      end,
      #{}, MetadataList).

redirect_to_logger(Severity, MetadataList, Format, Args, Sink) ->
    % TODO other sinks?
    if Severity =:= none ->
           ok;
       Sink =:= ?DEFAULT_SINK ->
           Metadata = logger_metadata(MetadataList),
           logger:log(Severity, Format, Args, Metadata);
       true ->
           {error, {bad_sink, Sink}}
    end.
