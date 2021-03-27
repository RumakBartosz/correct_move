-module(evaluation).

-export([evaluate/2]).


-spec evaluate(type:color(), type:tron_map()) -> integer().

evaluate(MyColor, Map) ->
    IAmLost = bot_utility:game_over(MyColor, Map),
    HeIsLost = bot_utility:game_over(bot_utility:negate_color(MyColor), Map),
    if
        IAmLost ->
            -1000;
        HeIsLost ->
            1000;
        true ->
            0
    end.

% get Voronoi diagram, check if there is distance 1 between any colored elements.
%-spec isClosed(type:tron_map()) -> boolean().

% compute moves, not euclidean distance
-spec getDistance(type:position(), type:position()) -> integer().

getDistance({Xone, Yone}, {Xtwo, Ytwo}) ->
    (Xone - Xtwo) + (Yone - Ytwo).

% start with bruteforce Voronoi, for every element of map, count distance to two heads.
-spec countCloser(type:tron_map()) -> [{blue | red, integer()}, ...].

countCloser(Map) ->
    countCloser(Map, [{blue, 0}, {red, 0}], 1).

-spec countCloser(type:tron_map(), CloserCounter, integer()) ->
                  CloserCounter when CloserCounter :: [{blue | red, integer()}, ...].

% TODO: Both functions to refactor
countCloser([], Acc, _VerticalAcc) ->
    Acc;
countCloser([Row|Rest] = Map, [{blue, CloserBlue}, {red, CloserRed}], VerticalAcc) ->
    BlueHead = map_utility:get_head(blue, Map),
    RedHead = map_utility:get_head(red, Map),
    [{blue, RowBlue}, {red, RowRed}] = countCloserVertical(Row,
                                                           BlueHead,
                                                           RedHead,
                                                           {1, VerticalAcc},
                                                           [{blue, 0}, {red, 0}]),
    countCloser(Rest, [{blue, CloserBlue + RowBlue}, {red, CloserRed + RowRed}], VerticalAcc + 1).

-spec countCloserVertical([type:tile()],
                           type:position(),
                           type:position(),
                           type:position(),
                           CloserCounter) ->
                           CloserCounter when CloserCounter :: [{blue | red, integer()}, ...].

countCloserVertical([_Tile|Tiles],
                    {BlueX, BlueY},
                    {RedX, RedY},
                    {TileX, TileY},
                    [{blue, CloserBlue}, {red, CloserRed}]) ->
    DistanceToBlue = getDistance({BlueX, BlueY}, {TileX, TileY}),
    DistanceToRed = getDistance({RedX, RedY}, {TileX, TileY}),
    case DistanceToBlue < DistanceToRed of
        true -> countCloserVertical(Tiles,
                                    {BlueX, BlueY},
                                    {RedX, RedY},
                                    {TileX, TileY},
                                    [{blue, CloserBlue + 1}, {red, CloserRed}]);
        false -> countCloserVertical(Tiles,
                                     {BlueX, BlueY},
                                     {RedX, RedY},
                                     {TileX, TileY},
                                     [{blue, CloserBlue}, {red, CloserRed + 1}])
    end.

