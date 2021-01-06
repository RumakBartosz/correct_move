-module(minimax_bot).
-compile([export_all]).
-compile([nowarn_export_all]).



%%====================================================================
%% API functions
%%====================================================================

%choose_move(_Map, _Color) ->
%    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%minimize(_Color, _Map) ->
%    ok.

%maximize(_Color, _Map) ->
%    ok.

%which_move_shall_i_take(_Color, _Map) ->
%    ok.

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

-spec alter_map(type:tron_map(), string(), type:coords()) -> type:tron_map().

alter_map(Map, Tile, {X, Y}) ->
    lists:sublist(Map, Y - 1) ++ [alter_vertical(lists:nth(Y, Map), Tile, X)] ++ lists:nthtail(Y, Map).

-spec alter_vertical(string(), string(), integer()) -> string().

alter_vertical(Row, Tile, X) ->
    lists:sublist(Row, X - 1) ++ Tile ++ lists:nthtail(X, Row).
