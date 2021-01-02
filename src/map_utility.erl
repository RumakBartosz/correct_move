-module(map_utility).

-export([available_moves/2,
         print_map/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec available_moves(type:color(), type:tron_map()) -> [type:move()] | [].

available_moves(Color, Map) ->
    lists:append([up_valid(Color, Map), down_valid(Color, Map),
        left_valid(Color, Map), right_valid(Color, Map)]).

-spec print_map(type:tron_map()) -> ok.

print_map([Head | _Rest] = Map) when is_list(Head) ->
    print_map(Map, [], map);
print_map([Head | _Rest] = Map) when is_number(Head) ->
    print_map(Map, [], map_string).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_head(type:color(), type:tron_map()) -> type:coords().

get_head(Color, Map) ->
    get_head(Color, Map, 1).

-spec get_head(type:color(), type:tron_map(), type:coord_y()) -> type:coords().

get_head(red, [], Acc) ->
    Acc;
get_head(red, [Head|Rest], Acc) ->
    case lists:member($R, Head) of
        true -> {Acc, get_head_vertical(red, Head, 1)};
        false -> get_head(red, Rest, Acc + 1)
    end;
get_head(blue, [], Acc) ->
    Acc;
get_head(blue, [Head|Rest], Acc) ->
    case lists:member($B, Head) of
        true -> {Acc, get_head_vertical(blue, Head, 1)};
        false -> get_head(blue, Rest, Acc + 1)
    end.

-spec get_head_vertical(type:color(), type:tron_map(), type:coord_x()) -> type:coord_x().

get_head_vertical(red, [], Acc) ->
    Acc;
get_head_vertical(red, [Head|Rest], Acc) ->
    case Head =:= $R of
        true -> Acc;
        false -> get_head_vertical(red, Rest, Acc + 1)
    end;
get_head_vertical(blue, [], Acc) ->
    Acc;
get_head_vertical(blue, [Head|Rest], Acc) ->
    case Head =:= $B of
        true -> Acc;
        false -> get_head_vertical(blue, Rest, Acc + 1)
    end.

-spec up_valid(type:color(), type:tron_map()) -> [type:move()] | [].

up_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X, lists:nth(Y - 1, Map)) of
        $\s -> [up];
        _Otherwise -> []
    end.

-spec down_valid(type:color(), type:tron_map()) -> [type:move()] | [].

down_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X, lists:nth(Y + 1, Map)) of
        $\s -> [down];
        _Otherwise -> []
    end.

-spec left_valid(type:color(), type:tron_map()) -> [type:move()] | [].

left_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X - 1, lists:nth(Y, Map)) of
        $\s -> [left];
        _Otherwise -> []
    end.

-spec right_valid(type:color(), type:tron_map()) -> [type:move()] | [].

right_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X + 1, lists:nth(Y, Map)) of
        $\s -> [right];
        _Otherwise -> []
    end.

-spec print_map(type:tron_map(), list(), atom()) -> ok.

print_map([], Acc, map) ->
    io:format("~n"),
    io:format(lists:reverse(Acc)),
    io:format("~n");
print_map([Head | Rest], Acc, map) ->
    print_map(Rest, [["  " ++ Head ++ "\n"] | Acc], map);
print_map(MapString, Acc, map_string) ->
    print_map(map_parser:parse(MapString), Acc, map).

