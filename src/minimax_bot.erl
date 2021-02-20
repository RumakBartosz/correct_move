-module(minimax_bot).
-compile([export_all]).
-compile([nowarn_export_all]).



%%====================================================================
%% API functions
%%====================================================================

-spec which_move_shall_i_take(type:tron_map(), type:color(), byte()) -> {type:move(), integer()}.

which_move_shall_i_take(Map, Color, Depth) ->
    map_utility:print_map(Map),
    % Helper function for mapping on all moves
    MoveFun = fun(Move) ->
                        -negamax(bot_utility:negate_color(Color),
                                bot_utility:make_move(Map, Color, Move),
                                Depth - 1,
                                0)
                end,

    % Main function, returning lowest value in subtree
    Moves = map_utility:available_moves(Color, Map),
    LowerValues = lists:map(MoveFun, Moves),
    MovesAndValues = lists:zip(Moves, LowerValues),
    TupleMinimum = fun({Move, Value}, {_AccMove, AccVal}) when Value > AccVal -> {Move, Value};
                      ({_Move, _Value}, {AccMove, AccVal}) -> {AccMove, AccVal} end,
    lists:foldl(TupleMinimum, {-1000, up}, MovesAndValues).

%%====================================================================
%% Internal functions
%%====================================================================

-spec negamax(type:color(), type:tron_map(), byte(), integer()) -> integer().

negamax(Color, Map, 0, Acc) ->
    case (Eval = evaluate(Color, Map)) < Acc of
        true -> Eval;
        false -> Acc
    end;
negamax(Color, Map, Depth, Acc) ->
    case bot_utility:game_over(Color, Map) of
        true -> -negamax(Color, Map, 0, Acc);
        false ->
            map_utility:print_map(Map),
            % Helper function for mapping on all moves
            MoveFun = fun(Move) ->
                          -negamax(bot_utility:negate_color(Color),
                                   bot_utility:make_move(Map, Color, Move),
                                   Depth - 1,
                                   Acc)
                    end,

            % Main function, returning lowest value in subtree
            Moves = map_utility:available_moves(Color, Map),
            LowerValues = lists:map(MoveFun, Moves),
            lists:min(LowerValues)
    end.

-spec evaluate(type:color(), type:tron_map()) -> integer().

evaluate(Color, Map) ->
    case bot_utility:game_over(Color, Map) of
        true -> -1000;
        false -> 1000
    end.

