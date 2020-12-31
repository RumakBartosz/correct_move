-module(correct_move).

-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    io:format(answerer:answer_interface(io:get_line(""))),
    io:format(answerer:answer_version(io:get_line(""))),

    %TODO: Improve color handling
    {Color, _} = answerer:answer_color(io:get_line("")),
    io:format("color ok\n"),

    loop(Color),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

loop(Color) ->
    io:format(answerer:answer_move(Color,
              map_parser:parse(lists:nth(2, string:split(io:get_line(""), " "))))),
    loop(Color).
