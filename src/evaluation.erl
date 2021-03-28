-module(evaluation).

-export([evaluate/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec evaluate(type:color(), type:tron_map()) -> integer().

evaluate(MyColor, Map) ->
    [{blue, BlueCloser}, {red, RedCloser}] = count_closer(Map),
    case MyColor =:= blue of
        true -> BlueCloser/RedCloser;
        false -> RedCloser/BlueCloser
    end.

%%====================================================================
%% Internal functions
%%====================================================================

% get Voronoi diagram, check if there is distance 1 between any colored elements.
%-spec isClosed(type:tron_map()) -> boolean().


% compute moves, not euclidean distance
-spec get_distance(type:position(), type:position()) -> integer().

get_distance({Xone, Yone}, {Xtwo, Ytwo}) ->
    (Xone - Xtwo) + (Yone - Ytwo).


% start with bruteforce, for every element of map count distance to two heads.
-spec count_closer(type:tron_map()) -> [{blue | red, integer()}, ...].

count_closer(Map) ->
    count_closer(Map, [{blue, 0}, {red, 0}], 1).


-spec count_closer(type:tron_map(), CloserCounter, integer()) ->
                  CloserCounter when CloserCounter :: [{blue | red, integer()}, ...].

% TODO: Both functions to refactor
count_closer([], Acc, _VerticalAcc) ->
    Acc;
count_closer([Row|Rest] = Map, [{blue, CloserBlue}, {red, CloserRed}], VerticalAcc) ->
    BlueHead = map_utility:get_head(blue, Map),
    RedHead = map_utility:get_head(red, Map),
    [{blue, RowBlue}, {red, RowRed}] = count_closer_vertical(Row,
                                                             BlueHead,
                                                             RedHead,
                                                             {1, VerticalAcc},
                                                             [{blue, 0}, {red, 0}]),
    count_closer(Rest, [{blue, CloserBlue + RowBlue}, {red, CloserRed + RowRed}], VerticalAcc + 1).


-spec count_closer_vertical([type:tile()],
                           type:position(),
                           type:position(),
                           type:position(),
                           CloserCounter) ->
                           CloserCounter when CloserCounter :: [{blue | red, integer()}, ...].

count_closer_vertical([], _BlueHead, _RedHead, _CurrentHead, Acc) ->
    Acc;
count_closer_vertical([_Tile|Tiles],
                    {BlueX, BlueY},
                    {RedX, RedY},
                    {TileX, TileY},
                    [{blue, CloserBlue}, {red, CloserRed}]) ->
    DistanceToBlue = get_distance({BlueX, BlueY}, {TileX, TileY}),
    DistanceToRed = get_distance({RedX, RedY}, {TileX, TileY}),
    case DistanceToBlue < DistanceToRed of
        true -> count_closer_vertical(Tiles,
                                    {BlueX, BlueY},
                                    {RedX, RedY},
                                    {TileX, TileY},
                                    [{blue, CloserBlue + 1}, {red, CloserRed}]);
        false -> count_closer_vertical(Tiles,
                                     {BlueX, BlueY},
                                     {RedX, RedY},
                                     {TileX, TileY},
                                     [{blue, CloserBlue}, {red, CloserRed + 1}])
    end.

