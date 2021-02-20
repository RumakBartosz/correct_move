-module(type).

-export_type([color/0, move/0, message/0, tron_map/0, coord_x/0, coord_y/0, coords/0]).

%%====================================================================
%% Type definitions
%%====================================================================

-type message() :: string() | move().

%TODO: Can this be an atom?
-type tile() :: $o | $\s | $r | $b | $R | $B.

-type color() :: blue | red.

-type move() :: up | down | left | right.

-type tron_map() :: [[tile()]].

-type coord_x() :: pos_integer().

-type coord_y() :: pos_integer().

-type coords() :: {coord_x(), coord_y()}.
