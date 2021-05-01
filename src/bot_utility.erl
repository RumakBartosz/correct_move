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

make_move(Map, Color, Move) ->
    {X, Y} = map_utility:get_head(Color, Map),
    Mark = get_mark_head(Color),
    Tail = get_mark_tail(Color),
    case Move of
        up ->
            Altered = alter_map(Map, Mark, {X, Y - 1}),
            alter_map(Altered, Tail, {X, Y});
        down ->
            Altered = alter_map(Map, Mark, {X, Y + 1}),
            alter_map(Altered, Tail, {X, Y});
        left ->
            Altered = alter_map(Map, Mark, {X - 1, Y}),
            alter_map(Altered, Tail, {X, Y});
        right ->
            Altered = alter_map(Map, Mark, {X + 1, Y}),
            alter_map(Altered, Tail, {X, Y})
    end.

-spec unmake_move(type:tron_map(), type:color(), type:move()) -> type:tron_map().

unmake_move(Map, Color, Move) ->
    {X, Y} = map_utility:get_head(Color, Map),
    Mark = get_mark_head(Color),
    case Move of
        up ->
            Altered = alter_map(Map, "o", {X, Y - 1}),
            alter_map(Altered, Mark, {X, Y});
        down ->
            Altered = alter_map(Map, "o", {X, Y + 1}),
            alter_map(Altered, Mark, {X, Y});
        left ->
            Altered = alter_map(Map, "o", {X - 1, Y}),
            alter_map(Altered, Mark, {X, Y});
        right ->
            Altered = alter_map(Map, "o", {X + 1, Y}),
            alter_map(Altered, Mark, {X, Y})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec alter_map(type:tron_map(), string(), type:position()) -> type:tron_map().

alter_map(Map, Tile, {X, Y}) ->
    lists:sublist(Map, Y - 1) ++
    [alter_vertical(lists:nth(Y, Map), Tile, X)] ++
    lists:nthtail(Y, Map).

-spec alter_vertical(string(), string(), type:coord()) -> string().

alter_vertical(Row, Tile, X) ->
    lists:sublist(Row, X - 1) ++ Tile ++ lists:nthtail(X, Row).

-spec get_mark_head(type:color()) -> [$R | $B].

get_mark_head(red) ->
    "R";
get_mark_head(blue) ->
    "B".

-spec get_mark_tail(type:color()) -> [$r | $b].

get_mark_tail(red) ->
    "r";
get_mark_tail(blue) ->
    "b".

