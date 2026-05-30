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

%% Helper module compiled with the parse transform so that
%% fake_lager_tests_transform_SUITE can observe the transform's output
%% without being transformed itself.
-module(fake_lager_tests_transform_helper).
-compile({parse_transform, lager_transform}).
-compile({lager_print_records_flag, true}).
-compile([nowarn_deprecated_catch]).

%% Record covering all three field forms so that all record_field_name/1 clauses
%% in lager_transform get exercised during compilation.
-record(helper_record, {plain_field, typed_field :: term(), default_field = default}).

-export([
    debug/2,
    info/2,
    new_helper_record/0,
    notice/2,
    warning/2,
    error/2,
    critical/2,
    alert/2,
    emergency/2,
    none/0,
    info_1arg/0,
    info_static_meta/0,
    info_dynamic_meta/1,
    info_literal_args/0,
    info_lazy_args/0,
    info_lazy_infix_op/1,
    info_lazy_prefix_op/1,
    info_lazy_case_expr/1,
    info_lazy_if_expr/1,
    info_lazy_receive_expr/0,
    info_in_case_body/0,
    info_in_nested_case_body/0,
    info_as_case_scrutinee/0,
    info_in_if_body/0,
    info_in_receive_body/0,
    info_as_receive_after_timeout/0,
    info_in_receive_after_body/0,
    info_in_try_body/0,
    info_in_catch_handler/0,
    info_in_try_after_body/0,
    info_in_begin_end/0,
    info_in_anon_fun/0,
    info_in_named_fun/0,
    info_in_list_comp_body/0,
    info_in_list_comp_generator/0,
    info_in_list_comp_filter/0,
    info_in_binary_comp/0,
    info_in_tuple/0,
    info_in_list_cons/0,
    info_in_map_construction/0,
    info_in_map_update/0,
    info_in_binary_segment/0,
    info_in_catch_expr/0,
    debug_unsafe/2,
    info_unsafe/2,
    warning_unsafe/2
]).

-ifdef(NATIVE_RECORDS).
-export([
    info_in_native_record_construction/0,
    info_in_native_record_update/0
]).
-endif.

new_helper_record() ->
    #helper_record{plain_field = undefined, typed_field = undefined, default_field = default}.

debug(Fmt, Args) -> lager:debug(Fmt, Args).
info(Fmt, Args) -> lager:info(Fmt, Args).
notice(Fmt, Args) -> lager:notice(Fmt, Args).
warning(Fmt, Args) -> lager:warning(Fmt, Args).
error(Fmt, Args) -> lager:error(Fmt, Args).
critical(Fmt, Args) -> lager:critical(Fmt, Args).
alert(Fmt, Args) -> lager:alert(Fmt, Args).
emergency(Fmt, Args) -> lager:emergency(Fmt, Args).

none() -> lager:none("should not be logged").

info_1arg() -> lager:info("hello world").
info_static_meta() -> lager:info([{request_id, <<"abc">>}], "handling request", []).
info_dynamic_meta(ExtraMeta) -> lager:info(ExtraMeta, "handling request", []).
info_literal_args() -> lager:info("~p", [hello]).
info_lazy_args() -> lager:info("~s", [erlang:integer_to_list(42)]).
info_lazy_infix_op(X) -> lager:info("~p", [X + 1]).
info_lazy_prefix_op(X) -> lager:info("~p", [-X]).
info_lazy_case_expr(X) ->
    lager:info("~p", [
        case X of
            foo -> bar;
            _ -> other
        end
    ]).
info_lazy_if_expr(X) ->
    lager:info("~p", [
        if
            X =:= foo -> bar;
            true -> other
        end
    ]).
info_lazy_receive_expr() ->
    lager:info("~p", [
        receive
            M -> M
        after 0 -> none
        end
    ]).

info_in_case_body() ->
    case ok of
        _ -> lager:info("in case body")
    end.

info_in_nested_case_body() ->
    case ok of
        _ ->
            case ok of
                _ -> lager:info("in nested case body")
            end
    end.

info_as_case_scrutinee() ->
    case lager:info("as case scrutinee") of
        _ -> ok
    end.

info_in_if_body() ->
    if
        true -> lager:info("in if body")
    end.

info_in_receive_body() ->
    self() ! fake_lager_test,
    receive
        fake_lager_test -> lager:info("in receive body")
    end.

info_as_receive_after_timeout() ->
    receive
        fake_lager_test -> ok
    after begin
        lager:info("as receive after timeout"),
        0
    end ->
        ok
    end.

info_in_receive_after_body() ->
    receive
        fake_lager_test -> ok
    after 0 ->
        lager:info("in receive after body")
    end.

info_in_try_body() ->
    try
        lager:info("in try body")
    catch
        _:_ -> ok
    end.

info_in_catch_handler() ->
    try
        error(fake_lager_test)
    catch
        _:_ -> lager:info("in catch handler")
    end.

info_in_try_after_body() ->
    try
        ok
    after
        lager:info("in try after body")
    end.

info_in_begin_end() ->
    begin
        lager:info("in begin end")
    end.

info_in_anon_fun() ->
    F = fun() -> lager:info("in anon fun") end,
    F().

info_in_named_fun() ->
    F = fun _Self() -> lager:info("in named fun") end,
    F().

info_in_list_comp_body() ->
    _ = [lager:info("in list comp body") || _ <- [x]],
    ok.

info_in_list_comp_generator() ->
    _ = [
        X
     || X <- begin
            lager:info("in list comp generator"),
            [x]
        end
    ],
    ok.

info_in_list_comp_filter() ->
    _ = [
        X
     || X <- [x],
        begin
            lager:info("in list comp filter"),
            true
        end
    ],
    ok.

info_in_binary_comp() ->
    _ = <<
        <<0>>
     || _ <- [x],
        begin
            lager:info("in binary comp"),
            true
        end
    >>,
    ok.

info_in_tuple() ->
    _ = {lager:info("in tuple"), ok},
    ok.

info_in_list_cons() ->
    _ = [lager:info("in list cons") | []],
    ok.

info_in_map_construction() ->
    _ = #{key => lager:info("in map construction")},
    ok.

info_in_map_update() ->
    M = #{key => old},
    _ = M#{key => lager:info("in map update")},
    ok.

info_in_binary_segment() ->
    _ = <<
        (begin
            lager:info("in binary segment"),
            0
        end)
    >>,
    ok.

info_in_catch_expr() ->
    _ = catch lager:info("in catch expr"),
    ok.

-ifdef(NATIVE_RECORDS).
% erlfmt-ignore
-record #fake_lager_nr{value}.

info_in_native_record_construction() ->
    _ = #fake_lager_nr{value = lager:info("in native record construction")},
    ok.

info_in_native_record_update() ->
    R = #fake_lager_nr{value = undefined},
    _ = R#fake_lager_nr{value = lager:info("in native record update")},
    ok.
-endif.

debug_unsafe(Fmt, Args) -> lager:debug_unsafe(Fmt, Args).
info_unsafe(Fmt, Args) -> lager:info_unsafe(Fmt, Args).
warning_unsafe(Fmt, Args) -> lager:warning_unsafe(Fmt, Args).
