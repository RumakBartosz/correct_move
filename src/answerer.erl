-module(answerer).

-export([answer/1,
         answer/2]).

-ifdef(bot).
    -if(?bot == rand_bot).
        -define(CURRENT_BOT, rand_bot).
    -elif(?bot == minimax_bot).
        -define(CURRENT_BOT, minimax_bot).
    -else.
        -define(CURRENT_BOT, minimax_bot).
    -endif.
-else.
    -define(CURRENT_BOT, minimax_bot).
-endif.

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
    Color.


-spec answer(atom(), type:color()) -> ok.

answer(move, Color) ->
    ["move", MoveString] = string:split(receive_message(), " "),
    send_message(answer_move(Color, map_parser:parse(MoveString))),
    send_message(terminate()).

%%====================================================================
%% Internal functions
%%====================================================================

-spec send_message(type:message()) -> ok.

send_message(IO) ->
    ok = io:put_chars(IO).


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


-spec answer_move(type:color(), type:tron_map()) -> type:message().

answer_move(Color, Map) ->
    atom_to_list(?CURRENT_BOT:choose_move(Color, Map)).

