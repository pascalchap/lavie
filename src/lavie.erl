-module (lavie).

-export ([start/0,periode/1]).

start() -> application:start(lavie).

periode(T) when is_integer(T) ->
	lavie_fsm:period(T).