-module(type).

-export_type([color/0, move/0, message/0, tile/0]).

%%====================================================================
%% Type definitions
%%====================================================================

-opaque message() :: string().

%TODO: Can this be an atom?
-opaque tile() :: $o | $\s | $r | $b | $R | $B.

-opaque color() :: blue | red.

%TODO: Try to restrict type to "up\n" | "down\n" | "left\n" | "right\n"
-opaque move() :: string().
