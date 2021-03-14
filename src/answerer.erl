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
    send_message("color ok" ++ terminate()),
    {ok, Color}.


-spec answer(atom(), type:color()) -> ok.

answer(move, Color) ->
    ["move", MoveString] = string:split(receive_message(), " "),
    send_message(answer_move(Color, map_parser:parse(MoveString))),
    send_message(terminate()).

%%====================================================================
%% Internal functions
%%====================================================================

-spec send_message(string() | type:move()) -> 'ok'.

send_message(IO) ->
    ok = io:format(IO).


-spec receive_message() -> type:message().

receive_message() ->
    io:get_line("").


-spec terminate() -> string().

terminate() ->
    io_lib:nl().


-spec answer_interface(type:message()) -> type:message().

answer_interface("tbi" ++ _Terminate) ->
    "tbi ok" ++ terminate().


-spec answer_version(type:message()) -> type:message().

answer_version("tbi v1" ++ _Terminate) ->
    "tbi v1 ok" ++ terminate().


-spec answer_color(type:message()) -> {ok, type:color()}.

answer_color("color blue" ++ _Terminate) ->
    {ok, blue};
answer_color("color red" ++ _Terminate) ->
    {ok, red}.


-spec answer_move(type:color(), type:tron_map()) -> type:move().

answer_move(Color, Map) ->
    minimax_bot:choose_move(Color, Map).

