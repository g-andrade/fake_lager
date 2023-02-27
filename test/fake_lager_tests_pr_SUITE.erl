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

-module(fake_lager_tests_pr_SUITE).
-compile({parse_transform, lager_transform}).

-ifndef(NO_CT_SUITE_BEHAVIOUR).
-behaviour(ct_suite).
-endif.

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% ct_suite Function Exports
%% ------------------------------------------------------------------

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% ------------------------------------------------------------------
%% Test Case Function Exports
%% ------------------------------------------------------------------

-export([superficial_record_test/1,
         deep_record_test/1,
         deep_data_structures_test/1,
         dont_compress_test/1,
         compress_test/1]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(good_state, {a, b = undefined, c = undefined :: term(), d}).
-record(bad_state, {}).
-record('Ugly.State', {zzz :: any() | term() | _}).

%% ------------------------------------------------------------------
%% ct_suite Function Definitions
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [{_Name = tests,
      _Opts = [parallel],
      _TestCases = [
        superficial_record_test,
        deep_record_test,
        deep_data_structures_test,
        dont_compress_test,
        compress_test
    ]}].

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

end_per_testcase(_TestCases, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Test Cases Function Definitions
%% ------------------------------------------------------------------

superficial_record_test(_Config) ->
    Record = #good_state{a = x, b = y, c = z, d = 3.14},
    ?assertEqual(
        {'#good_state', #{
            a => x,
            b => y,
            c => z,
            d => 3.14
        }},
        lager:pr(Record, ?MODULE)).

deep_record_test(_Config) ->
    DeepRecord = #'Ugly.State'{zzz = yyy},
    Record = #good_state{a = x, b = y, c = DeepRecord, d = "€€"},
    ?assertEqual(
        {'#good_state', #{
            a => x,
            b => y,
            c => {'#Ugly.State', #{zzz => yyy}},
            d => "€€"
        }},
        lager:pr(Record, ?MODULE)).

deep_data_structures_test(_Config) ->
    A = #good_state{a = 1},
    B = #good_state{b = 2},
    C = #good_state{c = 3},
    D = #bad_state{},
    Deep = [A, B, C, #{D => {A}, xyz => improper_list(C, D)}],

    ?assertMatch(
        [{'#good_state', #{a := 1, b := undefined}},
         {'#good_state', #{b := 2}},
         {'#good_state', #{c := 3}},
         #{{'#bad_state', #{}}
                := {{'#good_state', #{a := 1}}},
           xyz
                := [{'#good_state', #{c := 3}}
                    | {'#bad_state', #{}}]}
        ],
        lager:pr(Deep, ?MODULE)).

dont_compress_test(_Config) ->
    A = #good_state{a = 1},
    B = #good_state{b = 2},
    C = #good_state{c = 3},

    ?assertEqual(
        [{'#good_state', #{
            a => 1,
            b => undefined,
            c => undefined,
            d => undefined
         }},
         {'#good_state', #{
            a => undefined,
            b => 2,
            c => undefined,
            d => undefined
         }},
         {'#good_state', #{
            a => undefined,
            b => undefined,
            c => 3,
            d => undefined
         }}],
        lager:pr([A, B, C], ?MODULE, [{compress, false}])).

compress_test(_Config) ->
    A = #good_state{a = 1},
    B = #good_state{b = 2},
    C = #good_state{c = 3},

    ?assertEqual(
        [{'#good_state', #{
            a => 1
         }},
         {'#good_state', #{
            b => 2
         }},
         {'#good_state', #{
            c => 3
         }}],
        lager:pr([A, B, C], ?MODULE, [compress])).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-dialyzer({nowarn_function, improper_list/2}).
improper_list(A, B) ->
    [A | B].
