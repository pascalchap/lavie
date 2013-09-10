-module (lavie_server).

-behaviour(gen_server).

%%-include_lib("stdlib/include/ms_transform.hrl").
-compile({parse_transform,ms_transform}).

%% API
-export([start_link/0,
		start_link/2,
		get_neighbor/2,
		setBr/1,
		setSr/1,
		create_world/0,
		init_world/1,
		new_world/0,
		multicast/1,
		survive/2,
		die/2,
		state/2,
		birdth/0,
		info/0,
		update/0,
		done_info/0,
		done_update/0,
		born/0,
		born_finished/0,
		click/2,
		reset/0,
		config/0,
		save/1,
		read/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define (BR , 200).
-define (SR , 50).


-record(state, {init=false,br=?BR,sr=?SR,birdth,clean,alive,info=0,update=0,born=0}).

%%%===================================================================
%%% API
%%%===================================================================
setBr(N) when is_integer(N), N>4, N<1000 ->
	gen_server:cast(?SERVER,{bigRadius,N}).
setSr(N) when is_integer(N), N>4, N<1000 ->
	gen_server:cast(?SERVER,{smallRadius,N}).
create_world() ->
	gen_server:cast(?SERVER,create_world).
init_world(L) when is_list(L) ->
	gen_server:cast(?SERVER,{init_world,L}).
new_world() ->
	gen_server:cast(?SERVER,new_world).
get_neighbor(X,Y) ->
	gen_server:call(?SERVER,{get_neighbor,X,Y}).
state(X,Y) ->
	gen_server:call(?SERVER,{state,X,Y}).
multicast(Msg) ->
	gen_server:cast(?SERVER,{multicast,Msg}).
survive(X,Y) ->
	gen_server:cast(?SERVER,{survive,X,Y}).
die(X,Y) ->
	gen_server:cast(?SERVER,{die,X,Y}).
birdth() ->
	gen_server:cast(?SERVER,birdth).
done_info() ->
	gen_server:cast(?SERVER,done_info).
done_update() ->
	gen_server:cast(?SERVER,done_update).
info_finished() ->
	gen_server:cast(?SERVER,info_finished).
update_finished() ->
	gen_server:cast(?SERVER,update_finished).
born() ->
	gen_server:cast(?SERVER,born).
born_finished() ->
	gen_server:cast(?SERVER,born_finished).
info() ->
	multicast(info_neighbors).
update() ->
	multicast(update).
click(X,Y) -> 
	gen_server:cast(?SERVER,{toggle,X,Y}).
reset() ->
	multicast(reset).
config() ->
	gen_server:cast(?SERVER,config).
read(F) ->
	gen_server:call(?SERVER,{read,F}).
save(F) ->
	gen_server:call(?SERVER,{save,F}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
		start_link(?BR,?SR).

start_link(W,H) ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [W,H], []).

%%%===================================================================
%%% gen_server callbacks



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([W,H]) ->
	Birdth_MS = ets:fun2ms(fun({X,_,N,_,C}) when C == 3 -> {X,N} end),
	Clean_MS = ets:fun2ms(fun({X,_,_,_,N}) when N =/= 0 -> X end),
	Alive_MS = ets:fun2ms(fun({X,P,_,_,_}) when is_pid(P) -> P end),
	do_create_world(W,H),
    {ok, #state{clean=Clean_MS,birdth=Birdth_MS,alive=Alive_MS,br=W,sr=H}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%% {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_neighbor,X,Y}, _From, State) ->
		[{_,_,Neighbors}] = ets:lookup(cells,{X,Y}),
        {reply, {value,Neighbors}, State};
handle_call({state,X,Y}, _From, State) ->
		[Reply] = ets:lookup(cells,{X,Y}),
        {reply, {value,Reply}, State};
handle_call({save,F}, _From, State) ->
		MSS = ets:fun2ms(fun({X,P,_,A,_}) when is_pid(P) -> {X,A} end),
		A = ets:select(cells,MSS),
		Reply = file:write_file(F,io_lib:format("~p.",[A])),
        {reply, {value,Reply}, State};
handle_call({read,F}, _From, State) ->
		{ok,B} = file:read_file(F),
		{ok,Tokens,_} = erl_scan:string(binary_to_list(B)),
		{ok,Term} = erl_parse:parse_term(Tokens),
		Reply = load(Term,State#state.br,State#state.sr),
        {reply, {value,Reply}, State};		
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({bigRadius,N}, State) when State#state.init == false ->
    {noreply, State#state{br=N}};
handle_cast({smallRadius,N}, State) when State#state.init == false ->
    {noreply, State#state{sr=N}};
handle_cast(create_world, State) when State#state.init == false ->
	do_create_world(State#state.br,State#state.sr),
    {noreply, State};
handle_cast({toggle,X,Y}, State) -> %% when State#state.init == false ->
	do_toggle(X,Y),
    {noreply, State};
handle_cast({init_world,L}, State) when State#state.init == false ->
	do_init_world(L),
	lavie_wx:setcell(live,L),
	lavie_fsm:config_done(),
    {noreply, State#state{init=true}};
handle_cast(new_world, State) when State#state.init == true ->
	do_new_world(),
    {noreply, State#state{init=false}};
handle_cast({multicast,Msg}, #state{info=I,update=U,init=C} = State) ->
	NbCell = do_multicast(Msg,State#state.alive),
	{I1,U1,C1} = case {Msg,NbCell} of
		{info_neighbors,0} 	-> info_finished(), %% cas ou il n'y a plus de cellule vivante
							   {NbCell,U,C};
		{update,0} 			-> update_finished(), %% cas ou il n'y a plus de cellule vivante
							   {I,NbCell,C};
		{info_neighbors,_} 	-> {NbCell,U,C};
		{update,_} 			-> {I,NbCell,C};
		{reset,_} 			-> {I,U,false};
		_ 				-> {I,U,C}
	end,
    {noreply, State#state{info=I1,update=U1,init=C1}};
handle_cast(birdth, #state{birdth=MSB,clean=MSC} = State) ->
	B = do_birdth(MSB,MSC),
	case B of
		0 -> born_finished(); %% cas ou il n'y a pas de naissance
		_ -> ok
	end,
    {noreply, State#state{born=B}};
handle_cast(born, #state{born=B} = State) ->
	case B of
		1 -> born_finished();
		_ -> ok
	end,
    {noreply, State#state{born=B-1}};
handle_cast(done_info, #state{info=A} = State) ->
	case A of
		1 -> info_finished();
		_ -> ok
	end,
    {noreply, State#state{info=A-1}};
handle_cast(done_update, #state{update=A} = State) ->
	case A of
		1 -> update_finished();
		_ -> ok
	end,
    {noreply, State#state{update=A-1}};
handle_cast({survive,X,Y}, State) ->
	ets:update_counter(cells,{X,Y},{4,1}),
	ets:update_element(cells,{X,Y},{5,0}),
    {noreply, State};
handle_cast({die,X,Y}, State) ->
	lavie_wx:setcell(dead,{X,Y}),
    {noreply, State};
handle_cast(info_finished, State) when State#state.init == true ->
	lavie_fsm:info_finished(),
    {noreply, State};
handle_cast(update_finished, State) when State#state.init == true ->
	lavie_fsm:update_finished(),
    {noreply, State};
handle_cast(born_finished, State) when State#state.init == true ->
	lavie_fsm:born_finished(),
    {noreply, State};
handle_cast(config, State) ->
    {noreply, State#state{init=false}};
handle_cast(Msg, State) ->
	io:format("ignore message : ~p~n",[Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
		io:format("info server ~p~n",[State]),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_create_world(Br,Sr) ->
	ets:new(cells,[set,public,named_table,{write_concurrency,true},{read_concurrency,true}]),
	[ets:insert(cells,{{X,Y},empty,neighbors(X,Y,Br,Sr),0,0}) || X <- lists:seq(0,Br-1), Y <- lists:seq(0,Sr-1)].

do_init_world([]) -> ok;
do_init_world([{X,Y}|T]) ->
	[{{X,Y},empty,Neighbors,_Age,_N}] = ets:lookup(cells,{X,Y}),
	ets:insert(cells,{{X,Y},newCell(X,Y,Neighbors),Neighbors,0,0}),
	do_init_world(T).

do_new_world() ->
	killcell(ets:first(cells)),
	ets:delete(cells).

killcell('$end_of_table') -> ok;
killcell(K) ->
	[{K,P,_,_,_}] = ets:lookup(cells,K),
	cell:stop(P),
	killcell(ets:next(cells,K)).

do_multicast(Msg,MSA) ->
	A = ets:select(cells,MSA),
	lists:foreach(fun(P) -> cell:cast(P,Msg) end,A),
	length(A).

do_toggle(X,Y) ->
	[{_,P,Neighbors,_,_}] = ets:lookup(cells,{X,Y}),
	case P of
		empty -> 	newCell(X,Y,Neighbors),
					lavie_wx:setcell(live,{X,Y});
		_ -> 		cell:stop(P),
					lavie_wx:setcell(dead,{X,Y})
	end.

newCell(X,Y,Neighbors) ->
	{ok,Pid} = cell:start(X,Y,Neighbors),
	Pid.

wrap(Base,V) -> (V+Base) rem Base.

neighbors(X,Y,Br,Sr) ->
	[{wrap(Br,X1),wrap(Sr,Y1)} || {X1,Y1} <- [{X-1,Y-1},{X-1,Y},{X-1,Y+1},{X,Y-1},{X,Y+1},{X+1,Y-1},{X+1,Y},{X+1,Y+1}]].

do_birdth(MSB,MSC) ->
	B = ets:select(cells,MSB),
	setlive(B),
	C = ets:select(cells,MSC),
	lists:foreach(fun({X,Y}) -> ets:update_element(cells,{X,Y},[{4,0},{5,0}]) end,C),
	length(B).

load(Cells,B,S) ->
	load(Cells,B,S,[]).

load([],_,_,P) ->
	setlive(P);
load([{{X,Y},_}|Q],B,S,P) ->
	X1 = X rem B,
	Y1 = Y rem S,
	P1 = case ets:lookup(cells,{X1,Y1}) of
		[{_,empty,N,_,_}] -> [{{X1,Y1},N}|P];
		_ -> P
	end,
	load(Q,B,S,P1).

setlive(P) ->
	lists:foreach(fun({{X,Y},N}) -> cell:start(X,Y,N) end,P),
	lavie_wx:setcell(live,[X || {X,_} <- P]).