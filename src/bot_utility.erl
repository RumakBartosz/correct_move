-module(bot_utility).

-export([game_over/2,
         make_move/3,
         unmake_move/3,
         negate_color/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec negate_color(type:color()) -> type:color().

negate_color(blue) -> red;
negate_color(red) -> blue.

-spec game_over(type:color(), type:tron_map()) -> boolean().

game_over(Color, Map) ->
    map_utility:available_moves(Color, Map) =:= [].

-spec make_move(type:tron_map(), type:color(), type:move()) -> type:tron_map().

make_move(Map, red, Move) ->
    {X, Y} = map_utility:get_head(red, Map),
    case Move of
        up ->
            Altered = alter_map(Map, "R", {X, Y - 1}),
            alter_map(Altered, "r", {X, Y});
        down ->
            Altered = alter_map(Map, "R", {X, Y + 1}),
            alter_map(Altered, "r", {X, Y});
        left ->
            Altered = alter_map(Map, "R", {X - 1, Y}),
            alter_map(Altered, "r", {X, Y});
        right ->
            Altered = alter_map(Map, "R", {X + 1, Y}),
            alter_map(Altered, "r", {X, Y})
    end;
make_move(Map, blue, Move) ->
    {X, Y} = map_utility:get_head(blue, Map),
    case Move of
        up ->
            Altered = alter_map(Map, "B", {X, Y - 1}),
            alter_map(Altered, "b", {X, Y});
        down ->
            Altered = alter_map(Map, "B", {X, Y + 1}),
            alter_map(Altered, "b", {X, Y});
        left ->
            Altered = alter_map(Map, "B", {X - 1, Y}),
            alter_map(Altered, "b", {X, Y});
        right ->
            Altered = alter_map(Map, "B", {X + 1, Y}),
            alter_map(Altered, "b", {X, Y})
    end.

-spec unmake_move(type:tron_map(), type:color(), type:move()) -> type:tron_map().

unmake_move(Map, red, Move) ->
    {X, Y} = map_utility:get_head(red, Map),
    case Move of
        up ->
            Altered = alter_map(Map, "o", {X, Y - 1}),
            alter_map(Altered, "R", {X, Y});
        down ->
            Altered = alter_map(Map, "o", {X, Y + 1}),
            alter_map(Altered, "R", {X, Y});
        left ->
            Altered = alter_map(Map, "o", {X - 1, Y}),
            alter_map(Altered, "R", {X, Y});
        right ->
            Altered = alter_map(Map, "o", {X + 1, Y}),
            alter_map(Altered, "R", {X, Y})
    end;
unmake_move(Map, blue, Move) ->
    {X, Y} = map_utility:get_head(blue, Map),
    case Move of
        up ->
            Altered = alter_map(Map, "o", {X, Y - 1}),
            alter_map(Altered, "B", {X, Y});
        down ->
            Altered = alter_map(Map, "o", {X, Y + 1}),
            alter_map(Altered, "B", {X, Y});
        left ->
            Altered = alter_map(Map, "o", {X - 1, Y}),
            alter_map(Altered, "B", {X, Y});
        right ->
            Altered = alter_map(Map, "o", {X + 1, Y}),
            alter_map(Altered, "B", {X, Y})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec alter_map(type:tron_map(), string(), type:coords()) -> type:tron_map().

alter_map(Map, Tile, {X, Y}) ->
    lists:sublist(Map, Y - 1) ++ [alter_vertical(lists:nth(Y, Map), Tile, X)] ++ lists:nthtail(Y, Map).

-spec alter_vertical(string(), string(), integer()) -> string().

alter_vertical(Row, Tile, X) ->
    lists:sublist(Row, X - 1) ++ Tile ++ lists:nthtail(X, Row).
