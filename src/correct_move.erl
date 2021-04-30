-module(correct_move).

-export([main/1]).

-ignore_xref([main/1]).

% TODO: eunit (eval), gradualizer, proper evaluation
% TODO: specify same order of arguments in functions (very often color + map)
% TODO: get_distance should instead be count_moves_to_place

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().

main(_Args) ->
    answerer:answer(interface),
    answerer:answer(version),
    {ok, Color} = answerer:answer(color),

    loop(Color).

%%====================================================================
%% Internal functions
%%====================================================================

-spec loop(type:color()) -> no_return().

loop(Color) ->
    answerer:answer(move, Color),
    loop(Color).
