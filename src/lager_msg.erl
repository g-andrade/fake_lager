-module(lager_msg).

-include("lager.hrl").

-export([new/4, new/5]).
-export([message/1]).
-export([timestamp/1]).
-export([datetime/1]).
-export([severity/1]).
-export([severity_as_int/1]).
-export([metadata/1]).
-export([destinations/1]).

-ignore_xref(datetime/1).
-ignore_xref(destinations/1).
-ignore_xref(message/1).
-ignore_xref(metadata/1).
-ignore_xref(new/4).
-ignore_xref(new/5).
-ignore_xref(severity/1).
-ignore_xref(severity_as_int/1).
-ignore_xref(timestamp/1).

-opaque lager_msg() :: logger:log_event().
-export_type([lager_msg/0]).

-define(MEGA, 1000000).

%% create with provided timestamp, handy for testing mostly
-spec new(list(), erlang:timestamp(), lager:log_level(), [tuple()], list()) -> lager_msg().
new(_Msg, _Timestamp, none, _Metadata, _Destinations) ->
    error(nosup);
new(Msg, {MSec, Sec, USec}, Level, Metadata, _Destinations) ->
    Time = MSec + (Sec + (USec * ?MEGA) * ?MEGA),
    Meta = maps:put(time, Time, maps:from_list(Metadata)),
    #{level => Level, msg => {string, Msg}, meta => Meta}.

-spec new(list(), lager:log_level(), [tuple()], list()) -> lager_msg().
new(_Msg, none, _Metadata, _Destinations) ->
    error(nosup);
new(Msg, Level, Metadata, _Destinations) ->
    Time = logger:timestamp(),
    Meta = maps:put(time, Time, maps:from_list(Metadata)),
    #{level => Level, msg => {string, Msg}, meta => Meta}.

-spec message(lager_msg()) -> list().
message(#{msg := Msg, meta := Meta}) ->
    unicode:characters_to_list(normalize_msg(Msg, Meta)).

normalize_msg({report, Report}, #{report_cb := Cb}) when is_function(Cb, 1) ->
    Cb(Report);
normalize_msg({report, Report}, #{report_cb := Cb}) when is_function(Cb, 2) ->
    Cb(Report, #{});
normalize_msg({report, Report}, _Meta) ->
    [io_lib:fwrite("~w=~w", [Key, Value])
     || {Key, Value} <- maps:to_list(Report)];
normalize_msg({string, String}, _Meta) ->
    String;
normalize_msg({Format, Data}, _Meta)
  when is_atom(Format); is_list(Format); is_binary(Format) ->
    io_lib:fwrite(Format, Data).

-spec timestamp(lager_msg()) -> erlang:timestamp().
timestamp(#{meta := #{time := Timestamp}}) ->
    MicroSecs = Timestamp rem ?MEGA,
    Secs = Timestamp div ?MEGA,
    MegaSecs = Secs div ?MEGA,
    {MegaSecs, Secs rem ?MEGA, MicroSecs}.

-spec datetime(lager_msg()) -> {string(), string()}.
datetime(#{meta := #{time := Timestamp}}) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Timestamp, millisecond),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Y, Mo, D]),
    TimeStr = io_lib:format("~2..0B:~2..0B:~2..0B", [H, Mi, S]),
    {DateStr, TimeStr}.

-spec severity(lager_msg()) -> lager:log_level().
severity(#{level := Level}) ->
    Level.

-spec severity_as_int(lager_msg()) -> lager:log_level_number().
severity_as_int(#{level := Level}) ->
    ?LEVEL2NUM(Level).

-spec metadata(lager_msg()) -> [tuple()].
metadata(#{meta := Meta}) ->
    maps:to_list(Meta).

-spec destinations(lager_msg()) -> no_return().
-dialyzer({nowarn_function, destinations/1}).
destinations(_Msg) ->
    error(nosup).
