-module(answerer).

-export([answer/1,
         answer/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec answer(atom()) -> ok | {ok, type:color()}.

answer(interface) ->
    ok = io:format(answer_interface(io:get_line("")));
answer(version) ->
    ok = io:format(answer_version(io:get_line("")));
answer(color) ->
    {ok, Color} = answer_color(io:get_line("")),
    io:format("color ok\n"),
    {ok, Color}.

-spec answer(atom(), type:color()) -> ok.

answer(move, Color) ->
    ok = io:format(answer_move(Color,
                   map_parser:parse(lists:nth(2, string:split(io:get_line(""), " "))))),
    ok = io:format("~n").

%%====================================================================
%% Internal functions
%%====================================================================

-spec answer_interface(type:message()) -> type:message().

answer_interface("tbi\n") ->
    "tbi ok\n".

-spec answer_version(type:message()) -> type:message().

answer_version("tbi v1\n") ->
    "tbi v1 ok\n".

-spec answer_color(type:message()) -> {ok, type:color()}.

answer_color("color blue\n") ->
    {ok, blue};
answer_color("color red\n") ->
    {ok, red}.

-spec answer_move(type:color(), type:tron_map()) -> type:move().

answer_move(Color, Map) ->
    rand_bot:choose_move(Color, Map).
