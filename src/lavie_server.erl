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
		init_world/0,
		new_world/0,
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
		read/1,
		getrule/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define (BR , 200).
-define (SR , 50).


-record(state, {init=false,br=?BR,sr=?SR,birdth,clean,alive,info=0,update=0,born=0,livefun,birdthfun,rulelive=[{2,1},{3,1}],rulebirdth=[{3,1}]}).

%%%===================================================================
%%% API
%%%===================================================================
setBr(N) when is_integer(N), N>4, N<1000 ->
	gen_server:cast(?SERVER,{bigRadius,N}).
setSr(N) when is_integer(N), N>4, N<1000 ->
	gen_server:cast(?SERVER,{smallRadius,N}).
create_world() ->
	gen_server:cast(?SERVER,create_world).
init_world() ->
	gen_server:cast(?SERVER,init_world).
new_world() ->
	gen_server:cast(?SERVER,new_world).
get_neighbor(X,Y) ->
	gen_server:call(?SERVER,{get_neighbor,X,Y}).
state(X,Y) ->
	gen_server:call(?SERVER,{state,X,Y}).
multicast(Msg) ->
	gen_server:cast(?SERVER,{multicast,Msg}).
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
getrule() ->
	gen_server:call(?SERVER,getrule).

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
%% @spec init([Width,Height]) -> {ok, State}
%% -record(state, {	init=false, quand il vaut vrai, interdit la modification de la configuration (br,sr,livefun,birdthfun), plus vraiment utilise 
%%					br=?BR, grand rayon du tore
%%					sr=?SR, petit rayon do tore
%%					birdth, Match spec utilise pour selectionner les cellules eligibles pour une naissance
%%					clean, Match spec utilise pour nettoyer la table (cellule vide qui on un compte voisin non nul)
%%					alive, Match spec utilise pour selectionner les cellules vivantes
%%					info=0,	nb de messages done_info restant a recevoir
%%					update=0, nb de messages done_update restant a recevoir
%%					born=0, nb de messages born restant a recevoir
%%					livefun, fonction a passer aux cellules pour determiner si elles vont suvivre le prochain tour
%%					birdthfun}). foction pour determiner si une case elligible va voir une naissance
%% @end
%%--------------------------------------------------------------------
init([W,H]) ->
%	Rulea=[{2,1},{3,1}],
%	Ruleb=[{3,1}],
	random:seed(erlang:now()),
	Birdth_MS = ets:fun2ms(fun({X,empty,N,C}) when C >= 2, C =< 4 -> {X,N,C} end),
	Clean_MS = ets:fun2ms(fun({X,_,_,N}) when N =/= 0 -> X end),
	Alive_MS = ets:fun2ms(fun({X,P,_,_}) when is_pid(P) -> P end),
	do_create_world(W,H),
	State = #state{clean=Clean_MS,birdth=Birdth_MS,alive=Alive_MS,br=W,sr=H},
	Fa =fun({_,_,_,N}) -> live(N,State#state.rulelive) end,
	Fb =fun(X) -> birdth(X,State#state.rulebirdth) end,
    {ok, State#state{livefun=Fa,birdthfun=Fb}}.


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
		[{_,_,Neighbors,_}] = ets:lookup(cells,{X,Y}),
        {reply, {value,Neighbors}, State};
handle_call({state,X,Y}, _From, State) ->
		[Reply] = ets:lookup(cells,{X,Y}),
        {reply, {value,Reply}, State};
handle_call({save,F}, _From, State) ->
		MSS = ets:fun2ms(fun({X,P,_,A,_}) when is_pid(P) -> {X,A} end),
		A = ets:select(cells,MSS),
		Reply = file:write_file(F,io_lib:format("~p.",[A])),
        {reply, {value,Reply}, State};
handle_call({read,Fi}, _From, #state{livefun=F,br=Br,sr=Sr} = State) ->
		{ok,B} = file:read_file(Fi),
		{ok,Tokens,_} = erl_scan:string(binary_to_list(B)),
		{ok,Term} = erl_parse:parse_term(Tokens),
		Reply = load(Term,Br,Sr,F),
        {reply, {value,Reply}, State};		
handle_call(getrule, _From, State) ->
	io:format("getrule in server~n"),
		Reply = {State#state.rulelive,State#state.rulebirdth},
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
handle_cast({toggle,X,Y}, #state{livefun=F} = State) -> %% when State#state.init == false ->
	do_toggle(X,Y,F),
    {noreply, State};
handle_cast(init_world, State) when State#state.init == false ->
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
handle_cast(birdth, #state{birdth=MSB,clean=MSC,livefun=Fa,birdthfun=Fb} = State) ->
	B = do_birdth(MSB,MSC,Fa,Fb),
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
%% @private
%% @doc
%% structure d'un enregistrement dans la table ets cell
%% cle: {X,Y} les coordonnees de la cellule,
%% empty | Pid,
%% [neighbors],
%% Nb_neighbors_alive
%% @end

do_create_world(Br,Sr) ->
	ets:new(cells,[set,public,named_table,{write_concurrency,true},{read_concurrency,true}]),
	[ets:insert(cells,{{X,Y},empty,neighbors(X,Y,Br,Sr),0}) || X <- lists:seq(0,Br-1), Y <- lists:seq(0,Sr-1)].

do_new_world() ->
	killcell(ets:first(cells)),
	ets:delete(cells).

killcell('$end_of_table') -> ok;
killcell(K) ->
	[{K,P,_,_}] = ets:lookup(cells,K),
	cell:stop(P),
	killcell(ets:next(cells,K)).

do_multicast(Msg,MSA) ->
	A = ets:select(cells,MSA),
	lists:foreach(fun(P) -> cell:cast(P,Msg) end,A),
	length(A).

do_toggle(X,Y,F) ->
	[{_,P,Neighbors,_}] = ets:lookup(cells,{X,Y}),
	case P of
		empty -> 	newCell(X,Y,Neighbors,F);
		_ -> 		cell:stop(P)
	end.

newCell(X,Y,Neighbors,F) ->
	{ok,Pid} = cell(X,Y,Neighbors,F),
	Pid.

wrap(Base,V) -> (V+Base) rem Base.

neighbors(X,Y,Br,Sr) ->
	[{wrap(Br,X1),wrap(Sr,Y1)} || {X1,Y1} <- [{X-1,Y-1},{X-1,Y},{X-1,Y+1},{X,Y-1},{X,Y+1},{X+1,Y-1},{X+1,Y},{X+1,Y+1}]].

do_birdth(MSB,MSC,Fa,Fb) ->
	B = lists:filter(Fb,ets:select(cells,MSB)),
	L = setlive(B,Fa),
	C = ets:select(cells,MSC),
	lists:foreach(fun({X,Y}) -> ets:update_element(cells,{X,Y},[{4,0}]) end,C),
	L.

load(Cells,B,S,F) ->
	load(Cells,B,S,[],F).

load([],_,_,P,F) ->
	setlive(P,F);
load([{{X,Y},_}|Q],B,S,P,F) ->
	X1 = X rem B,
	Y1 = Y rem S,
	P1 = case ets:lookup(cells,{X1,Y1}) of
		[{_,empty,N,_}] -> [{{X1,Y1},N}|P];
		_ -> P
	end,
	load(Q,B,S,P1,F).

setlive(L,Fa) ->
	F = fun({P,N,_}) -> {P,N};(P) -> P end,
	L1 = [F(X) || X <- L],
	lists:foreach(fun({{X,Y},N}) -> cell(X,Y,N,Fa) end,L1),
	length(L1).

cell(X,Y,N,F) ->
	cell:start(X,Y,N,random:seed(),F).

birdth({_,_},_) -> true;
birdth({_,_,_N},[]) -> false;
birdth({_,_,N},[{N,P}|_]) -> random:uniform() =< P;
birdth(X,[_|T]) -> birdth(X,T).


live(_,[]) -> false;
live(N,[{N,P}|_]) -> random:uniform() =< P;
live(N,[_|T]) -> live(N,T).
