-module(minimax_tests).

-include_lib("eunit/include/eunit.hrl").

minimax_test_() ->
    [test_eval_on_game_over()].


test_eval_on_game_over() ->
    %Arrange
    Color = red,
    Map = test_utility:test_map_small(),
    Depth = 9,

    %Act
    Eval = minimax_bot:negamax(Color, Map, Depth, 0),

    %Assert
    ?_assertEqual([-1000], Eval).
