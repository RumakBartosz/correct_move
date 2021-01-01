-module(rand_bot).

-export([choose_move/2]).

%%====================================================================
%% API functions
%%====================================================================

choose_move(Color, Map) ->
    Moves = map_utility:available_moves(Color, Map),
    lists:nth(rand:uniform(length(Moves)), Moves).

