-module(minimax_bot).

-export([choose_move/2]).


-define(DEFAULT_DEPTH, 7).

%%====================================================================
%% API functions
%%====================================================================

-spec choose_move(type:color(), type:tron_map()) -> type:move().

choose_move(Color, Map) ->
    {Move, _Value} = which_move_shall_i_take(Map, Color, ?DEFAULT_DEPTH),
    Move.

%%====================================================================
%% Internal functions
%%====================================================================

-spec which_move_shall_i_take(type:tron_map(), type:color(), integer()) -> {type:move(), integer()}.

which_move_shall_i_take(Map, Color, Depth) ->
    % Helper function for mapping on all moves
    MoveFun = fun(Move) ->
                     maximize(Color, bot_utility:make_move(Map, Color, Move), Depth, Color)
              end,

    % Main function, returning lowest value in subtree
    Moves = map_utility:available_moves(Color, Map),
    LowerValues = lists:map(MoveFun, Moves),
    MovesAndValues = lists:zip(Moves, LowerValues),
    TupleMaximum = fun({Move, Value}, {_AccMove, AccVal}) when Value > AccVal -> {Move, Value};
                      ({_Move, _Value}, {AccMove, AccVal}) -> {AccMove, AccVal} end,
    lists:foldl(TupleMaximum, {up, -1001}, MovesAndValues).


-spec maximize(type:color(), type:tron_map(), integer(), type:color()) -> integer().

maximize(Color, Map, 0, MyColor) ->
    map_utility:write_map(Map),
    Eval = evaluation:evaluate(MyColor, Map),
    file:write_file("test.txt" ++ atom_to_list(Color),
                    "Eval from depth maximize: " ++
                     float_to_list(Eval) ++
                     io_lib:nl(), [append]),
    Eval;
maximize(Color, Map, Depth, MyColor) ->
    case bot_utility:game_over(Color, Map) of
        true ->
            map_utility:write_map(Map),
            Eval = evaluation:evaluate(MyColor, Map),
            file:write_file("test.txt" ++ atom_to_list(Color),
                            "Eval from end maximize: " ++
                             float_to_list(Eval) ++
                             io_lib:nl(), [append]),
            Eval;
        false ->
            % Helper function for mapping on all moves
            MoveFun = fun(Move) ->
                          minimize(bot_utility:negate_color(Color),
                                   bot_utility:make_move(Map, Color, Move),
                                   Depth - 1,
                                   MyColor)
                    end,

            % Main function, returning lowest value in subtree
            Moves = map_utility:available_moves(Color, Map),
            LowerValues = lists:map(MoveFun, Moves),
            lists:max(LowerValues)
    end.


-spec minimize(type:color(), type:tron_map(), integer(), type:color()) -> integer().

minimize(Color, Map, 0, MyColor) ->
    map_utility:write_map(Map),
    Eval = evaluation:evaluate(MyColor, Map),
    file:write_file("test.txt" ++ atom_to_list(Color),
                    "Eval from depth minimize: " ++
                     float_to_list(Eval) ++
                     io_lib:nl(), [append]),
    Eval;
minimize(Color, Map, Depth, MyColor) ->
    case bot_utility:game_over(Color, Map) of
        true ->
            map_utility:write_map(Map),
            Eval = evaluation:evaluate(MyColor, Map),
            file:write_file("test.txt" ++ atom_to_list(Color),
                            "Eval from end minimize: " ++
                             float_to_list(Eval) ++
                             io_lib:nl(), [append]),
            Eval;
        false ->
            % Helper function for mapping on all moves
            MoveFun = fun(Move) ->
                          maximize(bot_utility:negate_color(Color),
                                   bot_utility:make_move(Map, Color, Move),
                                   Depth - 1,
                                   MyColor)
                    end,

            % Main function, returning lowest value in subtree
            Moves = map_utility:available_moves(Color, Map),
            LowerValues = lists:map(MoveFun, Moves),
            lists:min(LowerValues)
    end.

