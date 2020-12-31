-module(rand_bot).
-export([choose_move/2]).
%TODO: move utility functions to map_utility module

choose_move(Color, Map) ->
    Moves = available_moves(Color, Map),
    lists:nth(rand:uniform(length(Moves)), Moves).

available_moves(Color, Map) ->
    up_valid(Color, Map) ++ down_valid(Color, Map) ++
    left_valid(Color, Map) ++ right_valid(Color, Map).

get_head(Color, Map) ->
    get_head(Color, Map, 1).

get_head("red", [], Acc) ->
    Acc;
get_head("red", [Head|Rest], Acc) ->
    case lists:member($R, Head) of
        true -> {Acc, get_head_vertical("red", Head, 1)};
        false -> get_head("red", Rest, Acc + 1)
    end;
get_head("blue", [], Acc) ->
    Acc;
get_head("blue", [Head|Rest], Acc) ->
    case lists:member($B, Head) of
        true -> {Acc, get_head_vertical("blue", Head, 1)};
        false -> get_head("blue", Rest, Acc + 1)
    end.

get_head_vertical("red", [], Acc) ->
    Acc;
get_head_vertical("red", [Head|Rest], Acc) ->
    case Head =:= $R of
        true -> Acc;
        false -> get_head_vertical("red", Rest, Acc + 1)
    end;
get_head_vertical("blue", [], Acc) ->
    Acc;
get_head_vertical("blue", [Head|Rest], Acc) ->
    case Head =:= $B of
        true -> Acc;
        false -> get_head_vertical("blue", Rest, Acc + 1)
    end.



up_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X, lists:nth(Y - 1, Map)) of
        $\s -> ["up\n"];
        _Otherwise -> []
    end.

down_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X, lists:nth(Y + 1, Map)) of
        $\s -> ["down\n"];
        _Otherwise -> []
    end.

left_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X - 1, lists:nth(Y, Map)) of
        $\s -> ["left\n"];
        _Otherwise -> []
    end.

right_valid(Color, Map) ->
    {Y, X} = get_head(Color, Map),
    case lists:nth(X + 1, lists:nth(Y, Map)) of
        $\s -> ["right\n"];
        _Otherwise -> []
    end.

