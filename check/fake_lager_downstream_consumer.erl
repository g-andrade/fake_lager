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

%% A representative downstream consumer of fake_lager: a module that opts into
%% the parse transform exactly as a real user would. It is compiled only under
%% the `check' rebar3 profile (see rebar.config) and exists purely so that the
%% static-analysis targets (dialyzer, xref) examine the transform's *output* -
%% the injected `logger:allow/2' + `logger:macro_log/5' calls - as they would
%% appear at a real downstream site. It is never part of the default build or
%% the published package, and is not meant to be called at runtime.
%%
%% Its raison d'etre: `logger:macro_log/5' is undocumented and its first
%% argument is typed `logger:location()' (which mandates the mfa/file/line
%% keys). This module guards against the transform emitting a call that breaks
%% that contract - which dialyzer/eqwalizer would flag for every downstream user.
-module(fake_lager_downstream_consumer).
-compile({parse_transform, lager_transform}).

%% Exported only to keep the call sites reachable; nothing actually invokes them.
-ignore_xref([{exercise, 0}, {exercise_dynamic_metadata, 2}]).

-export([exercise/0, exercise_dynamic_metadata/2]).

%% Every statically-known call shape the transform must handle, in one place.
-spec exercise() -> ok.
exercise() ->
    %% One-argument form (format string only).
    lager:info("one-argument form"),
    %% Two-argument form (format string + args).
    lager:info("two-argument form: ~p", [{a, b, c}]),
    %% Three-argument form with a literal metadata proplist (folded at compile time).
    lager:notice([{request_id, <<"static">>}], "three-argument static metadata", []),
    %% Every level, so each rewritten call site is type-checked.
    lager:debug("debug"),
    lager:info("info"),
    lager:notice("notice"),
    lager:warning("warning"),
    lager:error("error"),
    lager:critical("critical"),
    lager:alert("alert"),
    lager:emergency("emergency"),
    %% `none' is rewritten to `ok' (no logger call emitted).
    lager:none("never logged"),
    %% Custom sink (`audit' is declared via {lager_extra_sinks, ...} in the
    %% `check' profile), exercising the `lager_sink' metadata branch.
    audit:warning("custom sink").

%% Three-argument form with a metadata list that is not a compile-time literal,
%% which the transform merges at runtime via maps:merge/2.
-spec exercise_dynamic_metadata([{atom(), term()}], term()) -> ok.
exercise_dynamic_metadata(Metadata, Value) ->
    lager:info(Metadata, "three-argument dynamic metadata: ~p", [Value]).
