-module(correct_move).

-export([main/1]).

-ignore_xref([main/1]).
%TODO: change main to "message receive" function

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().

main(_Args) ->
    io:format(answerer:answer_interface(io:get_line(""))),
    io:format(answerer:answer_version(io:get_line(""))),

    {ok, Color} = answerer:answer_color(io:get_line("")),
    io:format("color ok\n"),

    loop(Color),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-spec loop(type:color()) -> no_return().

loop(Color) ->
    io:format(answerer:answer_move(Color,
              map_parser:parse(lists:nth(2, string:split(io:get_line(""), " "))))),
    loop(Color).
