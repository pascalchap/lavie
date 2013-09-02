-module (cell).

-behaviour(gen_server).

%% API
-export([start/3,
		get_neighbor/1,
		stop/1,
		cast/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record (state, {alive = false, x, y, neighbors=[]}).

%%%===================================================================
%%% API
%%%===================================================================
get_neighbor(P) when is_pid(P) ->
	gen_server:cast(P,get_neighbor).

stop(empty) ->
	empty;
stop(P) ->
	gen_server:cast(P,stop).

cast(empty,_Msg) -> empty;
cast(P,Msg) ->
	gen_server:cast(P,{cast,Msg}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(integer(),integer(),[tuple()]) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(X,Y,Neighbors) ->
        gen_server:start(?MODULE, [X,Y,Neighbors], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init([X,Y,Neighbors]) ->
		lavie_server:born(),
        {ok, #state{x=X,y=Y,neighbors=Neighbors}}.

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
handle_cast(get_neighbor, #state{x=X,y=Y} = State) ->
		{value,Neighbors} = lavie_server:get_neighbor(X,Y),
        {noreply, State#state{neighbors=Neighbors}};
handle_cast(stop,State) ->
		{stop, normal, State};
handle_cast({cast,reset}, #state{x=X,y=Y} = State) ->
		lavie_server:die(X,Y),
		{stop, normal, State};
handle_cast({cast,info_neighbors},#state{neighbors = Neighbors} = State) ->
		[ets:update_counter(cells,X,{5,1}) || X <- Neighbors],
		lavie_server:done_info(),
		{noreply, State};
handle_cast({cast,update}, #state{x=X,y=Y} = State) -> 
		R=do_update(X,Y,State),
		lavie_server:done_update(),
		R;
handle_cast(Msg, State) ->
		io:format("ignore ~p~n",[Msg]),
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

do_update(X,Y,State) ->
	{value,S} = lavie_server:state(X,Y),
	case live(S) of
		true -> lavie_server:survive(X,Y),
				{noreply, State};
		false -> lavie_server:die(X,Y),
				{stop, normal, State}
	end.

live({_,_,_,_,N}) when N > 1, N < 4 -> true;
live(_) -> false.
