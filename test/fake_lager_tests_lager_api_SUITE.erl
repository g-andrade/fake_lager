%% @copyright 2026 Guilherme Andrade
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

-module(fake_lager_tests_lager_api_SUITE).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% ct_suite Function Exports
%% ------------------------------------------------------------------

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% ------------------------------------------------------------------
%% Test Case Function Exports
%% ------------------------------------------------------------------

-export([
    md_returns_empty_list_when_unset/1,
    md_set_and_get/1,
    md_converts_module_function_to_mfa/1,
    md_with_non_list_raises_badarg/1,
    log_3_with_pid/1,
    log_3_with_metadata_list/1,
    log_4_with_format_and_args/1,
    log_5_with_bad_sink_returns_error/1,
    log_unsafe/1,
    none_severity_produces_no_event/1,
    dispatch_log_5_arg/1,
    dispatch_log_9_arg/1,
    do_log_9_arg/1,
    do_log_unsafe_10_arg/1,
    notsup_stubs_raise_error/1,
    start_and_stop/1,
    pr_without_parse_transform_returns_value_unchanged/1
]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CAPTURE_HANDLER, test_api_capture).
-define(LOG_TIMEOUT_MS, 500).

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-elvis([
    {elvis_style, simplify_anonymous_functions, disable}
]).

%% ------------------------------------------------------------------
%% ct_suite Function Definitions
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [
        {
            _Name = tests,
            _Opts = [],
            _TestCases = [
                md_returns_empty_list_when_unset,
                md_set_and_get,
                md_converts_module_function_to_mfa,
                md_with_non_list_raises_badarg,
                log_3_with_pid,
                log_3_with_metadata_list,
                log_4_with_format_and_args,
                log_5_with_bad_sink_returns_error,
                log_unsafe,
                none_severity_produces_no_event,
                dispatch_log_5_arg,
                dispatch_log_9_arg,
                do_log_9_arg,
                do_log_unsafe_10_arg,
                notsup_stubs_raise_error,
                start_and_stop,
                pr_without_parse_transform_returns_value_unchanged
            ]
        }
    ].

init_per_suite(Config) ->
    PrimaryConfig = logger:get_primary_config(),
    ok = logger:set_primary_config(level, all),
    [{original_primary_level, maps:get(level, PrimaryConfig)} | Config].

end_per_suite(Config) ->
    OrigLevel = proplists:get_value(original_primary_level, Config),
    ok = logger:set_primary_config(level, OrigLevel),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    logger:unset_process_metadata(),
    ok = logger:add_handler(?CAPTURE_HANDLER, fake_lager_test_capture, #{
        config => #{owner => self()},
        level => all
    }),
    Config.

end_per_testcase(_TestCase, _Config) ->
    logger:unset_process_metadata(),
    _ = logger:remove_handler(?CAPTURE_HANDLER),
    ok.

%% ------------------------------------------------------------------
%% Test Case Function Definitions
%% ------------------------------------------------------------------

md_returns_empty_list_when_unset(_Config) ->
    ?assertEqual([], lager:md()).

md_set_and_get(_Config) ->
    ok = lager:md([{key, value}]),
    ?assertEqual([{key, value}], lager:md()).

md_converts_module_function_to_mfa(_Config) ->
    ok = lager:md([{module, my_mod}, {function, my_fun}]),
    ?assertEqual([{mfa, {my_mod, my_fun, 0}}], lager:md()).

md_with_non_list_raises_badarg(_Config) ->
    ?assertError(badarg, lager:md(not_a_list)).

log_3_with_pid(_Config) ->
    ok = lager:log(info, self(), "test message"),
    ?assertMatch(#{level := info}, receive_log_event()).

log_3_with_metadata_list(_Config) ->
    ok = lager:log(info, [{key, val}], "test message"),
    ?assertMatch(#{level := info}, receive_log_event()).

log_4_with_format_and_args(_Config) ->
    ok = lager:log(info, [], "~p", [hello]),
    #{msg := Msg} = receive_log_event(),
    ?assertEqual({"~p", [hello]}, Msg).

log_5_with_bad_sink_returns_error(_Config) ->
    ?assertEqual(
        {error, {bad_sink, bad_sink}},
        lager:log(bad_sink, info, [], "msg", [])
    ).

log_unsafe(_Config) ->
    ok = lager:log_unsafe(warning, [{key, val}], "~p", [hello]),
    ?assertMatch(#{level := warning}, receive_log_event()).

none_severity_produces_no_event(_Config) ->
    ok = lager:log(none, [], "should not be logged", []),
    expect_no_log_event().

dispatch_log_5_arg(_Config) ->
    ok = lager:dispatch_log(info, [], "test", [], 4096),
    ?assertMatch(#{level := info}, receive_log_event()).

dispatch_log_9_arg(_Config) ->
    ok = lager:dispatch_log(info, ?MODULE, test, 1, self(), [], "test", [], 4096),
    ?assertMatch(#{level := info}, receive_log_event()).

do_log_9_arg(_Config) ->
    ok = lager:do_log(info, [], "test ~p", [hello], 4096, -1, -1, [], self()),
    #{msg := Msg} = receive_log_event(),
    ?assertEqual({"test ~p", [hello]}, Msg).

do_log_unsafe_10_arg(_Config) ->
    ok = lager:do_log_unsafe(
        warning, [], "test ~p", [hello], 4096, -1, -1, [], lager_event, self()
    ),
    #{msg := Msg} = receive_log_event(),
    ?assertEqual({"test ~p", [hello]}, Msg).

notsup_stubs_raise_error(_Config) ->
    NotsupCalls = [
        fun() -> lager:clear_all_traces() end,
        fun() -> lager:clear_trace_by_destination(x) end,
        fun() -> lager:get_loglevel(x) end,
        fun() -> lager:get_loglevel(x, y) end,
        fun() -> lager:get_loglevels(x) end,
        fun() -> lager:install_trace(x, y) end,
        fun() -> lager:install_trace(x, y, z) end,
        fun() -> lager:list_all_sinks() end,
        fun() -> lager:posix_error(x) end,
        fun() -> lager:pr_stacktrace([]) end,
        fun() -> lager:pr_stacktrace([], {error, reason}) end,
        fun() -> lager:remove_trace(x) end,
        fun() -> lager:rotate_all() end,
        fun() -> lager:rotate_handler(x) end,
        fun() -> lager:rotate_handler(x, y) end,
        fun() -> lager:rotate_sink(x) end,
        fun() -> lager:safe_format(x, y, z) end,
        fun() -> lager:safe_format_chop(x, y, z) end,
        fun() -> lager:set_loghwm(x, y) end,
        fun() -> lager:set_loghwm(x, y, z) end,
        fun() -> lager:set_loghwm(x, y, z, w) end,
        fun() -> lager:set_loglevel(x, y) end,
        fun() -> lager:set_loglevel(x, y, z) end,
        fun() -> lager:set_loglevel(x, y, z, w) end,
        fun() -> lager:status() end,
        fun() -> lager:stop_trace({x, y, z}) end,
        fun() -> lager:stop_trace(x, y, z) end,
        fun() -> lager:trace(x, y) end,
        fun() -> lager:trace(x, y, z) end,
        fun() -> lager:trace_console(x) end,
        fun() -> lager:trace_console(x, y) end,
        fun() -> lager:trace_file(x, y) end,
        fun() -> lager:trace_file(x, y, z) end,
        fun() -> lager:trace_file(x, y, z, w) end,
        fun() -> lager:trace_func(x, y, z) end,
        fun() -> lager:trace_state(x, y, z) end,
        fun() -> lager:unsafe_format(x, y) end,
        fun() -> lager:update_loglevel_config(x) end
    ],
    lists:foreach(
        fun(Call) -> ?assertError(notsup, Call()) end,
        NotsupCalls
    ).

start_and_stop(_Config) ->
    ok = lager:start(),
    ok = application:stop(lager).

pr_without_parse_transform_returns_value_unchanged(_Config) ->
    Value = {record_looking_tuple, field1, field2},
    ?assertEqual(Value, lager:pr(Value, ?MODULE)).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

receive_log_event() ->
    receive
        {log_capture, Event} -> Event
    after ?LOG_TIMEOUT_MS ->
        ct:fail("No log event received within ~p ms", [?LOG_TIMEOUT_MS])
    end.

expect_no_log_event() ->
    receive
        {log_capture, _} = Unexpected ->
            ct:fail("Unexpected log event: ~p", [Unexpected])
    after 100 ->
        ok
    end.
