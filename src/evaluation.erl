-module(evaluation).

-export([evaluate/2]).
-compile(export_all).
-compile(nowarn_export_all).

%%====================================================================
%% API functions
%%====================================================================

-spec evaluate(type:color(), type:tron_map()) -> integer().

evaluate(MyColor, Map) ->
    HisColor = bot_utility:negate_color(MyColor),
    [{blue, BlueCloser}, {red, RedCloser}] = count_closer(Map),
    DistanceMap = case MyColor =:= blue of
                      true -> (BlueCloser + 1)/(RedCloser + 1);
                      false -> (RedCloser + 1)/(BlueCloser + 1)
                  end,
    AmIDone = case bot_utility:game_over(MyColor, Map) of
                  true -> -10;
                  false -> 0
              end,

    IsHeDone = case bot_utility:game_over(HisColor, Map) of
                   true -> 10;
                   false -> 0
               end,

    AreWeTooClose = case get_distance(map_utility:get_head(MyColor, Map),
                                      map_utility:get_head(HisColor, Map)) == 1 of
                        true -> -1;
                        false -> 0
                    end,

    DistanceMap + AmIDone + IsHeDone + AreWeTooClose.

%%====================================================================
%% Internal functions
%%====================================================================

% get Voronoi diagram, check if there is distance 1 between any colored elements.
%-spec isClosed(type:tron_map()) -> boolean().


% compute moves, not euclidean distance
-spec get_distance(type:position(), type:position()) -> integer().

get_distance({Xone, Yone}, {Xtwo, Ytwo}) ->
    abs((Xone - Xtwo) + (Yone - Ytwo)).


% start with bruteforce, for every element of map count distance to two heads.
-spec count_closer(type:tron_map()) -> [{blue | red, integer()}, ...].

count_closer(Map) ->
    BlueHead = map_utility:get_head(blue, Map),
    RedHead = map_utility:get_head(red, Map),
    count_closer(Map, BlueHead, RedHead, [{blue, 0}, {red, 0}], 1).


% TODO: Both functions to refactor
-spec count_closer(type:tron_map(), type:position(), type:position(), CloserCounter, integer()) ->
                  CloserCounter when CloserCounter :: [{blue | red, integer()}, ...].

count_closer([], _BlueHead, _RedHead, Acc, _VerticalAcc) ->
    Acc;
count_closer([Row|Rest], BlueHead, RedHead, [{blue, CloserBlue}, {red, CloserRed}], VerticalAcc) ->
    [{blue, RowBlue}, {red, RowRed}] = count_closer_vertical(Row,
                                                             BlueHead,
                                                             RedHead,
                                                             {1, VerticalAcc},
                                                             [{blue, 0}, {red, 0}]),
    count_closer(Rest,
                 BlueHead,
                 RedHead,
                 [{blue, CloserBlue + RowBlue}, {red, CloserRed + RowRed}],
                 VerticalAcc + 1).


% TODO: If moves are simultaneous, there should be DistanceToBlue equals DistanceToRed case
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
                                    {TileX + 1, TileY},
                                    [{blue, CloserBlue + 1}, {red, CloserRed}]);
        false -> count_closer_vertical(Tiles,
                                     {BlueX, BlueY},
                                     {RedX, RedY},
                                     {TileX + 1, TileY},
                                     [{blue, CloserBlue}, {red, CloserRed + 1}])
    end.

