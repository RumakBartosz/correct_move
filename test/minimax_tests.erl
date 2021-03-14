-module(minimax_tests).

-include_lib("eunit/include/eunit.hrl").

minimax_test_() ->
    [test_eval_on_one_move(),
     test_clearly_better_move_down(),
     test_clearly_better_move_right(),
     test_clearly_better_move_up()].


test_eval_on_one_move() ->
    %Arrange
    Color = red,
    Map = test_utility:test_map_one_move(),

    %Act
    Move = minimax_bot:choose_move(Color, Map),

    %Assert
    ?_assertEqual(right, Move).

test_clearly_better_move_down() ->
    %Arrange
    Color = blue,
    Map = test_utility:test_map_better_move_down(),

    %Act
    Move = minimax_bot:choose_move(Color, Map),

    %Assert
    ?_assertEqual(down, Move).

test_clearly_better_move_right() ->
    %Arrange
    Color = blue,
    Map = test_utility:test_map_better_move_down(),

    %Act
    Move = minimax_bot:choose_move(Color, Map),

    %Assert
    ?_assertEqual(right, Move).

test_clearly_better_move_up() ->
    %Arrange
    Color = blue,
    Map = test_utility:test_map_better_move_down(),

    %Act
    Move = minimax_bot:choose_move(Color, Map),

    %Assert
    ?_assertEqual(up, Move).

%TODO: try testing full minimax bot with ct.

