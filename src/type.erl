-module(type).

-export_type([color/0, move/0, message/0, tron_map/0, position/0, coord/0, tile/0, tron_map_row/0]).

%%====================================================================
%% Type definitions
%%====================================================================

-type message() :: string().

%TODO: Can this be an atom?
-type tile() :: $o | $\s | $r | $b | $R | $B.

-type color() :: blue | red.

-type move() :: up | down | left | right.

-type tron_map() :: [tron_map_row(), ...].

-type tron_map_row() :: [tile(), ...].

-type coord() :: pos_integer().

-type position() :: {coord(), coord()}.

