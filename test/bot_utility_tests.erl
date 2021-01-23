-module(bot_utility_tests).

-include_lib("eunit/include/eunit.hrl").

bot_utility_test_() ->
    [make_move(),
     make_move_blue(),
     make_complicated_move()].

make_move() ->
    %Arrange
    Map = test_utility:test_map_medium(),

    %Act
    AlteredMap = bot_utility:make_move(Map, red, down),

    %Assert
    ?_assertEqual(AlteredMap,     ["oooooo",
                                   "or   o",
                                   "oR   o",
                                   "o    o",
                                   "o   Bo",
                                   "oooooo"]).

make_move_blue() ->
    %Arrange
    Map = test_utility:test_map_medium(),

    %Act
    AlteredMap = bot_utility:make_move(Map, blue, up),

    %Assert
    ?_assertEqual(AlteredMap,     ["oooooo",
                                   "oR   o",
                                   "o    o",
                                   "o   Bo",
                                   "o   bo",
                                   "oooooo"]).

make_complicated_move() ->
    %Arrange
    Map = test_utility:test_map_medium(),

    %Act
    AlteredMap1 = bot_utility:make_move(Map, red, down),
    AlteredMap2 = bot_utility:make_move(AlteredMap1, blue, up),
    AlteredMap3 = bot_utility:make_move(AlteredMap2, red, right),
    AlteredMap4 = bot_utility:make_move(AlteredMap3, blue, left),

    %Assert
    ?_assertEqual(AlteredMap4,     ["oooooo",
                                   "or   o",
                                   "orR  o",
                                   "o  Bbo",
                                   "o   bo",
                                   "oooooo"]).
