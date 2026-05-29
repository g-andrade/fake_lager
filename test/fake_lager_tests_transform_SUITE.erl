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

%% Tests for the lager_transform parse transform.
%% This suite intentionally does NOT use the parse transform — all lager calls
%% are made through fake_lager_tests_transform_helper, which is transformed.
%% This keeps the test assertions free from any transform side-effects.
-module(fake_lager_tests_transform_SUITE).

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
    all_levels_produce_logger_events/1,
    none_level_produces_no_logger_event/1,
    metadata_contains_mfa/1,
    metadata_contains_file/1,
    metadata_contains_line/1,
    one_arg_form_is_supported/1,
    two_arg_form_is_supported/1,
    three_arg_form_with_static_metadata/1,
    three_arg_form_with_dynamic_metadata/1,
    literal_args_produce_immediate_message/1,
    call_in_args_produces_lazy_message/1,
    unsafe_level_aliases_produce_logger_events/1,
    pr_context_embedded_for_records/1
]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CAPTURE_HANDLER, test_transform_capture).
-define(LOG_TIMEOUT_MS, 500).
-define(H, fake_lager_tests_transform_helper).

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-elvis([
    {elvis_style, no_invalid_dynamic_calls, disable}
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
                all_levels_produce_logger_events,
                none_level_produces_no_logger_event,
                metadata_contains_mfa,
                metadata_contains_file,
                metadata_contains_line,
                one_arg_form_is_supported,
                two_arg_form_is_supported,
                three_arg_form_with_static_metadata,
                three_arg_form_with_dynamic_metadata,
                literal_args_produce_immediate_message,
                call_in_args_produces_lazy_message,
                unsafe_level_aliases_produce_logger_events,
                pr_context_embedded_for_records
            ]
        }
    ].

init_per_suite(Config) ->
    % Compile the helper from source at runtime so that lager_transform:parse_transform/2
    % executes while the cover tool is active, making the transform's code paths measurable.
    HelperSrc = filename:join(filename:dirname(?FILE), "fake_lager_tests_transform_helper.erl"),
    {ok, fake_lager_tests_transform_helper, Binary} =
        compile:file(HelperSrc, [binary, nowarn_missing_spec]),
    {module, _} = code:load_binary(fake_lager_tests_transform_helper, HelperSrc, Binary),
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
    ok = logger:add_handler(?CAPTURE_HANDLER, fake_lager_test_capture, #{
        config => #{owner => self()},
        level => all
    }),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = logger:remove_handler(?CAPTURE_HANDLER),
    ok.

%% ------------------------------------------------------------------
%% Test Case Function Definitions
%% ------------------------------------------------------------------

all_levels_produce_logger_events(_Config) ->
    ?H:debug("test ~p", [debug]),
    ?assertEqual(debug, maps:get(level, receive_log_event())),
    ?H:info("test ~p", [info]),
    ?assertEqual(info, maps:get(level, receive_log_event())),
    ?H:notice("test ~p", [notice]),
    ?assertEqual(notice, maps:get(level, receive_log_event())),
    ?H:warning("test ~p", [warning]),
    ?assertEqual(warning, maps:get(level, receive_log_event())),
    ?H:error("test ~p", [error]),
    ?assertEqual(error, maps:get(level, receive_log_event())),
    ?H:critical("test ~p", [critical]),
    ?assertEqual(critical, maps:get(level, receive_log_event())),
    ?H:alert("test ~p", [alert]),
    ?assertEqual(alert, maps:get(level, receive_log_event())),
    ?H:emergency("test ~p", [emergency]),
    ?assertEqual(emergency, maps:get(level, receive_log_event())).

none_level_produces_no_logger_event(_Config) ->
    ?assertEqual(ok, ?H:none()),
    expect_no_log_event().

metadata_contains_mfa(_Config) ->
    ?H:info("test", []),
    #{meta := Meta} = receive_log_event(),
    ?assertMatch(
        {fake_lager_tests_transform_helper, info, 2},
        maps:get(mfa, Meta)
    ).

metadata_contains_file(_Config) ->
    ?H:info("test", []),
    #{meta := Meta} = receive_log_event(),
    File = maps:get(file, Meta),
    ?assert(is_list(File)),
    ?assert(lists:suffix("fake_lager_tests_transform_helper.erl", File)).

metadata_contains_line(_Config) ->
    ?H:info("test", []),
    #{meta := Meta} = receive_log_event(),
    ?assert(is_integer(maps:get(line, Meta))).

one_arg_form_is_supported(_Config) ->
    ?H:info_1arg(),
    #{msg := Msg} = receive_log_event(),
    ?assertEqual({"hello world", []}, Msg).

two_arg_form_is_supported(_Config) ->
    ?H:info("~p items", [42]),
    #{msg := Msg} = receive_log_event(),
    ?assertEqual({"~p items", [42]}, Msg).

three_arg_form_with_static_metadata(_Config) ->
    ?H:info_static_meta(),
    #{meta := Meta} = receive_log_event(),
    ?assertMatch(#{mfa := _, request_id := <<"abc">>}, Meta).

three_arg_form_with_dynamic_metadata(_Config) ->
    ?H:info_dynamic_meta([{request_id, <<"xyz">>}]),
    #{meta := Meta} = receive_log_event(),
    ?assertMatch(#{mfa := _, request_id := <<"xyz">>}, Meta).

literal_args_produce_immediate_message(_Config) ->
    ?H:info_literal_args(),
    #{msg := Msg} = receive_log_event(),
    ?assertEqual({"~p", [hello]}, Msg).

call_in_args_produces_lazy_message(_Config) ->
    ?H:info_lazy_args(),
    #{msg := Msg} = receive_log_event(),
    % The transform wraps expensive args in a lazy fun; OTP 29+ evaluates the
    % fun before dispatching to handlers, so the handler may receive either.
    ExpectedContent = {"~s", ["42"]},
    case Msg of
        {F, no_args} when is_function(F, 1) -> ?assertEqual(ExpectedContent, F(no_args));
        ExpectedContent -> ok
    end.

unsafe_level_aliases_produce_logger_events(_Config) ->
    ?H:debug_unsafe("~p", [debug]),
    ?assertEqual(debug, maps:get(level, receive_log_event())),
    ?H:info_unsafe("~p", [info]),
    ?assertEqual(info, maps:get(level, receive_log_event())),
    ?H:warning_unsafe("~p", [warning]),
    ?assertEqual(warning, maps:get(level, receive_log_event())).

pr_context_embedded_for_records(_Config) ->
    Record = ?H:new_helper_record(),
    Result = lager:pr(Record, ?H),
    ?assertMatch(
        {'#helper_record', #{
            plain_field := undefined,
            typed_field := undefined,
            default_field := default
        }},
        Result
    ).

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
