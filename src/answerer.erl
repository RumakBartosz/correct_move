-module(answerer).

-export([answer/1,
         answer/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec answer(atom()) -> ok | {ok, type:color()}.

answer(interface) ->
    send_message(answer_interface(receive_message()));
answer(version) ->
    send_message(answer_version(receive_message()));
answer(color) ->
    {ok, Color} = answer_color(receive_message()),
    send_message("color ok\n"),
    {ok, Color}.

-spec answer(atom(), type:color()) -> ok.

answer(move, Color) ->
    send_message(answer_move(Color,
                   map_parser:parse(lists:nth(2, string:split(io:get_line(""), " "))))),
    send_message("~n").

%%====================================================================
%% Internal functions
%%====================================================================

-spec send_message(string()) -> 'ok'.

send_message(IO) ->
    ok = io:format(IO).

-spec receive_message() -> 'eof' | binary() | string() | {'error', _}.

receive_message() ->
    io:get_line("").

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
