-module(minimax_bot).
-compile([export_all]).
-compile([nowarn_export_all]).



%%====================================================================
%% API functions
%%====================================================================

%=spec which_move_shall_i_take(type:tron_map(), type:color()) -> type:move().

%which_move_shall_i_take(_Map, _Color) ->
%    ok.

%%====================================================================
%% Internal functions
%%====================================================================


%TODO: As negamax should return one value, change it to foldl eg.
%TODO: Problem with passing Acc value, not traversing.
-spec negamax(type:color(), type:tron_map(), byte(), integer()) -> [integer()].

negamax(Color, Map, 0, Acc ) ->
    case (Eval = evaluate(Color, Map)) < Acc of
        true -> [Eval];
        false -> [Acc]
    end;
negamax(Color, Map, Depth, Acc) ->
    GoDeeper = fun(Move) ->
                   map_utility:print_map(bot_utility:make_move(Map, Color, Move)),
                   Evaluation = lists:map(fun erlang:'-'/1,
                                          negamax(bot_utility:negate_color(Color),
                                                  bot_utility:make_move(Map, Color, Move),
                                                  Depth - 1,
                                                  Acc)
                                         ),
                   case Evaluation < Acc of
                       true -> Evaluation;
                       false -> Acc
                   end
               end,
    case bot_utility:game_over(Color, Map) of
        true -> negamax(Color, Map, 0, Acc);
        false ->
            lists:map(GoDeeper, map_utility:available_moves(Color, Map))
    end.

-spec evaluate(type:color(), type:tron_map()) -> integer().

evaluate(Color, Map) ->
    case bot_utility:game_over(Color, Map) of
        true -> -1000;
        false -> 1000
    end.

