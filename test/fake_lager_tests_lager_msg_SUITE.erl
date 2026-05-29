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

-module(fake_lager_tests_lager_msg_SUITE).

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
    new_4_creates_msg/1,
    new_5_creates_msg_with_timestamp/1,
    new_4_none_level_raises/1,
    new_5_none_level_raises/1,
    message_from_string/1,
    message_from_format_and_args/1,
    message_from_report_with_fun1_cb/1,
    message_from_report_with_fun2_cb/1,
    message_from_report_without_cb/1,
    timestamp_returns_valid_tuple/1,
    datetime_returns_strings/1,
    severity_returns_level/1,
    severity_as_int_returns_integer/1,
    metadata_returns_key_value_list/1,
    destinations_raises/1
]).

%% ------------------------------------------------------------------
%% Static Check Tweaks
%% ------------------------------------------------------------------

-elvis([
    {elvis_style, dont_repeat_yourself, disable}
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
            _Opts = [parallel],
            _TestCases = [
                new_4_creates_msg,
                new_5_creates_msg_with_timestamp,
                new_4_none_level_raises,
                new_5_none_level_raises,
                message_from_string,
                message_from_format_and_args,
                message_from_report_with_fun1_cb,
                message_from_report_with_fun2_cb,
                message_from_report_without_cb,
                timestamp_returns_valid_tuple,
                datetime_returns_strings,
                severity_returns_level,
                severity_as_int_returns_integer,
                metadata_returns_key_value_list,
                destinations_raises
            ]
        }
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ------------------------------------------------------------------
%% Test Case Function Definitions
%% ------------------------------------------------------------------

new_4_creates_msg(_Config) ->
    Msg = lager_msg:new("hello", info, [{key, val}], []),
    ?assertMatch(#{level := info, msg := {string, "hello"}, meta := _}, Msg).

new_5_creates_msg_with_timestamp(_Config) ->
    Msg = lager_msg:new("hello", {0, 0, 0}, info, [{key, val}], []),
    ?assertMatch(#{level := info, msg := {string, "hello"}, meta := _}, Msg).

new_4_none_level_raises(_Config) ->
    ?assertError(nosup, lager_msg:new("hello", none, [], [])).

new_5_none_level_raises(_Config) ->
    ?assertError(nosup, lager_msg:new("hello", {0, 0, 0}, none, [], [])).

message_from_string(_Config) ->
    Msg = lager_msg:new("hello world", info, [], []),
    ?assertEqual("hello world", lager_msg:message(Msg)).

message_from_format_and_args(_Config) ->
    Msg = #{level => info, msg => {"~p items", [42]}, meta => #{}},
    ?assertEqual("42 items", lager_msg:message(Msg)).

message_from_report_with_fun1_cb(_Config) ->
    Cb = fun(Report) -> io_lib:format("~p", [Report]) end,
    Msg = #{level => info, msg => {report, #{a => 1}}, meta => #{report_cb => Cb}},
    ?assert(is_list(lager_msg:message(Msg))).

message_from_report_with_fun2_cb(_Config) ->
    Cb = fun(Report, _Opts) -> io_lib:format("~p", [Report]) end,
    Msg = #{level => info, msg => {report, #{a => 1}}, meta => #{report_cb => Cb}},
    ?assert(is_list(lager_msg:message(Msg))).

message_from_report_without_cb(_Config) ->
    Msg = #{level => info, msg => {report, #{a => 1}}, meta => #{}},
    ?assert(is_list(lager_msg:message(Msg))).

timestamp_returns_valid_tuple(_Config) ->
    Msg = lager_msg:new("test", info, [], []),
    {MegaSec, Sec, MicroSec} = lager_msg:timestamp(Msg),
    ?assert(is_integer(MegaSec) andalso MegaSec >= 0),
    ?assert(is_integer(Sec) andalso Sec >= 0 andalso Sec < 1000000),
    ?assert(is_integer(MicroSec) andalso MicroSec >= 0 andalso MicroSec < 1000000).

datetime_returns_strings(_Config) ->
    Msg = lager_msg:new("test", info, [], []),
    {DateStr, TimeStr} = lager_msg:datetime(Msg),
    ?assert(is_list(lists:flatten(DateStr))),
    ?assert(is_list(lists:flatten(TimeStr))).

severity_returns_level(_Config) ->
    Msg = lager_msg:new("test", warning, [], []),
    ?assertEqual(warning, lager_msg:severity(Msg)).

severity_as_int_returns_integer(_Config) ->
    Msg = lager_msg:new("test", warning, [], []),
    N = lager_msg:severity_as_int(Msg),
    ?assert(is_integer(N) andalso N > 0).

metadata_returns_key_value_list(_Config) ->
    Msg = lager_msg:new("test", info, [{key, val}], []),
    MetaList = lager_msg:metadata(Msg),
    ?assert(is_list(MetaList)),
    ?assert(lists:keymember(key, 1, MetaList)).

destinations_raises(_Config) ->
    Msg = lager_msg:new("test", info, [], []),
    ?assertError(nosup, lager_msg:destinations(Msg)).
