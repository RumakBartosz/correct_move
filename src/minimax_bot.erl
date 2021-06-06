-module(minimax_bot).

-export([choose_move/2]).

-ifdef(debug).
-define(LOG(Map, Color, Eval, Function, Context),
            file:write_file("log/test-" ++ atom_to_list(Color) ++ ".txt",
                            "Eval from " ++ atom_to_list(Function) ++ ": " ++ float_to_list(Eval) ++
                            " context: " ++ Context ++ io_lib:nl() ++ "For map:"
                            ++ io_lib:nl(), [append]),
            map_utility:write_map(Map, "log/test-" ++ atom_to_list(Color) ++ ".txt")
       ).

-else.
-define(LOG(Map, Color, Eval, Function, Context),
        Color).

-endif.

-define(DEFAULT_DEPTH, 7).

%%====================================================================
%% API functions
%%====================================================================

-spec choose_move(type:color(), type:tron_map()) -> type:move().

choose_move(Color, Map) ->
    {Move, _Value} = which_move_shall_i_take(Color, Map, ?DEFAULT_DEPTH),
    Move.

%%====================================================================
%% Internal functions
%%====================================================================

-spec which_move_shall_i_take(type:color(), type:tron_map(), integer()) -> {type:move(), integer()}.

which_move_shall_i_take(Color, Map, Depth) ->
    % Helper function for mapping on all moves
    MoveFun = fun(Move) ->
                     negamax(Color, bot_utility:make_move(Map, Color, Move), Depth, Color)
              end,

    % Main function, returning lowest value in subtree
    Moves = map_utility:available_moves(Color, Map),
    LowerValues = lists:map(MoveFun, Moves),
    MovesAndValues = lists:zip(Moves, LowerValues),
    TupleMaximum = fun({Move, Value}, {_AccMove, AccVal}) when Value > AccVal -> {Move, Value};
                      ({_Move, _Value}, {AccMove, AccVal}) -> {AccMove, AccVal} end,
    lists:foldl(TupleMaximum, {up, -1001}, MovesAndValues).


-spec negamax(type:color(), type:tron_map(), integer(), type:color()) -> float().

negamax(Color, Map, 0, MyColor) ->
    Eval = evaluation:evaluate(MyColor, Map),
    ?LOG(Map, Color, Eval, ?FUNCTION_NAME, "Depth 0"),
    Eval;
negamax(Color, Map, Depth, MyColor) ->
    case bot_utility:game_over(Color, Map) of
        true ->
            Eval = evaluation:evaluate(MyColor, Map),
            ?LOG(Map, Color, Eval, ?FUNCTION_NAME, "Depth " ++ integer_to_list(Depth)),
            Eval;
        false ->
            % Helper function for mapping on all moves
            MoveFun = fun(Move) ->
                          -negamax(bot_utility:negate_color(Color),
                                   bot_utility:make_move(Map, Color, Move),
                                   Depth - 1,
                                   MyColor)
                      end,

            % Main function, returning lowest value in subtree
            Moves = map_utility:available_moves(Color, Map),
            LowerValues = lists:map(MoveFun, Moves),
            lists:max(LowerValues)
    end.

