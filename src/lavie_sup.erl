
-module(lavie_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([W,H]) ->
	Lavie = {lavie,{lavie_server,start_link, [W,H]}, 
	    transient,1000,worker,[lavie_server]}, 
	Gui = {gui,{lavie_wx,start_link, [W,H]}, 
	    transient,1000,worker,[lavie_wx]}, 
	Fsm = {fsm,{lavie_fsm,start_link, []}, 
	    transient,1000,worker,[lavie_fsm]}, 
	{ok, { {one_for_one, 5, 10}, [Gui,Lavie,Fsm]} }.

