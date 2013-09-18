-module(lavie_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [W1,H1,Z1]) ->
	W = get(width,W1), 
	H = get(height,H1),
	Z = get(zoom,Z1),	
    lavie_sup:start_link([W,H,Z]).

stop(_State) ->
	% init:stop().
	ok.
		
get(Name,Def) ->
	case init:get_argument(Name) of
		{ok,[[L]]} -> list_to_integer(L);
		_ -> Def
	end.
