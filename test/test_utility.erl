-module(test_utility).

-export([test_map_string_null/0, test_map_string_small/0,
         test_map_string_medium/0, test_map_string_large/0]).
-export([test_map_null/0, test_map_small/0,
         test_map_medium/0, test_map_large/0]).

test_map_string_null() ->
    "".

test_map_string_small() ->
    "oooo/oRBo/oooo".

test_map_string_medium() ->
    "oooooo/oR3o/o4o/o4o/o3Bo/oooooo".

test_map_string_large() ->
    "oooooooooooo/oB9o/o10o/o10o/o10o/o10o/o10o/o10o/o10o/o10o/o9Ro/oooooooooooo".

test_map_null() ->
    [""].

test_map_small() ->
    ["oooo",
     "oRBo",
     "oooo"].

test_map_medium() ->
    ["oooooo",
     "oR   o",
     "o    o",
     "o    o",
     "o   Bo",
     "oooooo"].

test_map_large() ->
    ["oooooooooooo",
     "oB         o",
     "o          o",
     "o          o",
     "o          o",
     "o          o",
     "o          o",
     "o          o",
     "o          o",
     "o          o",
     "o         Ro",
     "oooooooooooo"].
