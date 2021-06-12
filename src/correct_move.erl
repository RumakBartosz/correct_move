-module(correct_move).

-export([main/1]).

-ignore_xref([main/1]).

% TODO: eunit (eval), gradualizer, proper evaluation
% TODO: get_distance should instead be count_moves_to_place
% TODO: compilation variable to eval depth
% TODO: propEr tests
% TODO: fix all warnings
% TODO: fix Color warning in LOG
% TODO: make logging path independent (always in ../log)

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().

main(_Args) ->
    answerer:answer(interface),
    answerer:answer(version),
    Color = answerer:answer(color),

    loop(Color).

%%====================================================================
%% Internal functions
%%====================================================================

-spec loop(type:color()) -> no_return().

loop(Color) ->
    answerer:answer(move, Color),
    loop(Color).

