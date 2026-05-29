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
    debug_unsafe/2,
    info_unsafe/2,
    warning_unsafe/2
]).

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

debug_unsafe(Fmt, Args) -> lager:debug_unsafe(Fmt, Args).
info_unsafe(Fmt, Args) -> lager:info_unsafe(Fmt, Args).
warning_unsafe(Fmt, Args) -> lager:warning_unsafe(Fmt, Args).
