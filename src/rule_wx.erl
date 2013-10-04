-module (rule_wx).

-export ([start_link/0]).

-include_lib("wx/include/wx.hrl").
-include("../include/lavie.hrl").

-behaviour(wx_object).

-record (state, {frame}).

-define (SERVER , ?MODULE).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, handle_event/2, 
     terminate/2, code_change/3,handle_sync_event/3]).

start_link() ->
    Pid = wx_object:start_link({local,?SERVER},?MODULE, [], []),
    {ok,Pid}.

init([]) ->
	changerule().


handle_sync_event(_Event, _wxObj, _State) ->
    ok.

handle_info(_M,S) -> 
    {noreply, S}.

handle_event(#wx{event=#wxClose{}},#state{frame=F} = S) ->
	lavie_wx:enable(),
	wxFrame:destroy(F),
    {stop, normal, S};
handle_event(_E,S) ->
    {noreply,S}.

handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.
    
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(Reason, _State) ->
    io:format("terminate rule gui with reason = ~p~n",[Reason]),
    normal.




changerule() ->
	lavie_server:getrule(),
    T = wxFrame:new(wx:null(), ?CHILD, "Edition des regles", 
    							[{size,{400,200}},
                                {style,	
                                 	?wxSYSTEM_MENU bor
                                 	?wxCAPTION  bor
                                 	?wxCLOSE_BOX
                                 }]),
    wxFrame:connect(T, close_window), 
    wxWindow:show(T),
    {T,#state{frame=T}}.
