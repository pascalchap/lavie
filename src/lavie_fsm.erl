%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module (lavie_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
         config_done/0,
         info_finished/0,
         update_finished/0,
         born_finished/0,
         idle/2,
         dclick/0,
         right_down/0,
         middle_down/0,
         faster/0,
         slower/0,
         save/1,
         read/1
         ]).

%% gen_fsm callbacks
-export([init/1,
         config/2,
         wait_info_finished/2,
         wait_update_finished/2,
         wait_born_finished/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {endCycle=false,cycle=20,standby=false,pidcycle,do_save=false,do_read=false,generation=0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

config_done() ->
        gen_fsm:send_event(?SERVER,config_done).

info_finished() ->
        gen_fsm:send_event(?SERVER,info_finished).

update_finished() ->
        gen_fsm:send_event(?SERVER,update_finished).

born_finished() ->
        gen_fsm:send_event(?SERVER,born_finished).        

dclick() -> 
        gen_fsm:send_all_state_event(?SERVER,dclick).       

right_down() -> 
        gen_fsm:send_all_state_event(?SERVER,right_down).  

middle_down() ->
        gen_fsm:send_all_state_event(?SERVER,middle_down).  

faster() ->
        gen_fsm:send_all_state_event(?SERVER,faster).  

slower() ->
        gen_fsm:send_all_state_event(?SERVER,slower).

save(N) ->
        gen_fsm:send_all_state_event(?SERVER,{save,N}).

read(N) ->
        gen_fsm:send_all_state_event(?SERVER,{read,N}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%% {ok, StateName, State, Timeout} |
%% ignore |
%% {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        lavie_wx:info(config),
        lavie_server:init_world(),
        {ok, config, #state{standby = true}}.

%%--------------------------------------------------------------------
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% generic spec state_name(Event, State) ->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
config(config_done, #state{generation=A} = State) ->
        lavie_server:info(),
        lavie_wx:refresh(),
        Pid = newCycle(State#state.cycle),
        lavie_wx:info(io_lib:format("marche    -> generation ~p",[A])),
       {next_state, wait_info_finished, State#state{pidcycle=Pid}}.

wait_info_finished(info_finished, State) ->
        lavie_server:update(),
        {next_state, wait_update_finished, State}.

wait_update_finished(update_finished, State) ->
        lavie_server:birdth(),
        {next_state, wait_born_finished, State}.

wait_born_finished(born_finished, #state{generation=A,standby=true} = State) ->
        State#state.pidcycle ! stop,
        case State#state.do_read of
                false -> false;
                F1 -> do_read(F1)
        end,
        case State#state.do_save of
                false -> false;
                F2 -> lavie_server:save(F2)
        end,
        lavie_wx:refresh(),
        lavie_wx:refreshtoggle(true),
        lavie_wx:info(io_lib:format("standby   -> generation ~p",[A])),
        {next_state, standby, State#state{do_read=false,do_save=false}};
wait_born_finished(born_finished, State) when   State#state.endCycle == true ->
        lavie_wx:refresh(),
        wait_20ms(),
        {next_state, idle, State#state{endCycle=false}};
wait_born_finished(born_finished, State) ->
        lavie_wx:refresh(),
        {next_state, wait_cycle, State}.

idle(done20ms,#state{generation=A} = State) ->
        lavie_server:info(),
        lavie_wx:refreshtoggle(false),
        Pid = newCycle(State#state.cycle),
        {next_state, wait_info_finished, State#state{pidcycle=Pid,generation=A+1}}.



%%--------------------------------------------------------------------
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(dclick, config, State) ->
        lavie_server:init_world(),
        lavie_wx:info(io_lib:format("config    -> generation 0",[])),
        {next_state, config, State#state{standby=false}};
handle_event(dclick, standby, #state{generation=A} = State) ->
        lavie_wx:info(io_lib:format("marche    -> generation ~p",[A])),
        idle(),
        {next_state, idle, State#state{standby=false}};
handle_event(dclick, StateName, State) ->
        {next_state, StateName, State#state{standby=true}};


handle_event(middle_down, standby, State) ->
        lavie_server:config(),
        lavie_wx:info(io_lib:format("config    -> generation 0",[])),
        {next_state, config, State#state{standby=false,generation=0}};
handle_event(middle_down, config, State) ->
        lavie_server:reset(),
        {next_state, config, State};

handle_event(right_down, standby, State) ->
        idle(),
        {next_state, idle, State#state{standby=true}};

handle_event(faster, StateName, #state{cycle = P} = State) ->
        NewP = max(round(P/1.5),2),
        {next_state, StateName, State#state{cycle=NewP}};

handle_event(slower, StateName, #state{cycle = P} = State) ->
        NewP = round(P*1.5),
        {next_state, StateName, State#state{cycle=NewP}};

handle_event(cycle, wait_cycle, State) ->
        idle(),
        {next_state, idle, State#state{endCycle=false}};
handle_event(cycle, standby, State) ->
        % il y a une course entre l'arret de cycle et l'entree dans standby
        {next_state, standby, State#state{endCycle=false}};
handle_event(cycle, StateName, State) ->
        {next_state, StateName, State#state{endCycle=true}};

handle_event({save,F}, StateName, State) when StateName == config; StateName == standby ->
        lavie_server:save(F),
        {next_state, StateName, State};
handle_event({save,F}, StateName, State) ->
        {next_state, StateName, State#state{do_save=F,standby=true}};

handle_event({read,F}, StateName, State) when StateName == config; StateName == standby ->
        do_read(F),
        {next_state, StateName, State};
handle_event({read,F}, StateName, State) ->
        {next_state, StateName, State#state{do_read=F,standby=true}};

handle_event(Event, StateName, #state{cycle=C}) ->
        M = io_lib:format("Ignore Event ~p during state ~p",[Event,StateName]),
        lavie_wx:info(M),
        {next_state, StateName, #state{endCycle=true,cycle=C}}.

%%--------------------------------------------------------------------
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {reply, Reply, NextStateName, NextState} |
%% {reply, Reply, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState} |
%% {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
        Reply = ok,
        {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%% {next_state, NextStateName, NextState} |
%% {next_state, NextStateName, NextState, Timeout} |
%% {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
        io:format("fsm in state ~p with data ~p~n",[StateName,State]),
        {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%% {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
        {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
wait_20ms() ->
        spawn(fun () -> receive
                        after 1 -> gen_fsm:send_event(lavie_fsm,done20ms)
                        end
                end).

newCycle(T) ->
        spawn(fun () -> receive 
                                stop -> ok
                        after T -> gen_fsm:send_all_state_event(lavie_fsm,cycle) 
                        end
                end).

idle() ->
        wait_20ms().


do_read(F) ->
        lavie_server:read(F).