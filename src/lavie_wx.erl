-module (lavie_wx).
%%
%% Include files
%%

-include_lib("wx/include/wx.hrl").
-include("../include/lavie.hrl").

-behaviour(wx_object).

-define (SERVER , ?MODULE).

-record(state,{frame   %% la fentre principale
			,panel     %% la zonne d'affichage des cellule
            ,memoryDC  %% Device contexte en memoire pour preparer les operations de rafraichissement
			,bitmap    %% le contenu a afficher
			,w         %% largeur
			,h         %% hauteur
			,penlive   %% crayon pour dessiner les cellules vivantes
			,pendead   %% crayon pour dessiner les cellules mortes
			,brushlive %% crayon pour dessiner les cellules vivantes
			,brushdead %% crayon pour dessiner les cellules mortes
			,zoom	   %% taille des cellules
			,cellbuf   = [] %% liste des cellules en attente de rafraichissement
			,refreshtoggle = true
			}).

%%
%% Exported Functions
%%
-export([start_link/0
		,start_link/3
		,setcell/2
		,info/1
		,enable/0
		,refresh/0
		,refreshtoggle/1
		]).

%%
%% callback Functions
%%
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, handle_event/2, 
     terminate/2, code_change/3,handle_sync_event/3]).

%%
%% API Functions
%%
start_link() ->
	start_link(200,50,2).
start_link(W,H,Z) ->
    wx:new(),
    % {wx_ref,_Id,_WxType,Pid} = wx_object:start_link(?MODULE, [W,H,Z], [{debug,[trace]}]),
    {wx_ref,_Id,_WxType,Pid} = wx_object:start_link(?MODULE, [W,H,Z], []),
    register(?SERVER,Pid),
	io:format("start_link lavie_wx; pid = ~p, server = ~p~n",[Pid,?SERVER]),
    {ok,Pid}.

init([W,H,Z]) ->
    {Frame, Panel, Bitmap, MDC, PV, PM, BV, BM} = wx:batch(fun() -> create_window(W,H,Z) end),
    {Frame, #state{frame=Frame, panel=Panel, bitmap=Bitmap, memoryDC=MDC, w=(Z+1)*W+1, h=(Z+1)*H+1, zoom = Z, 
    penlive=PV, pendead=PM, brushlive=BV, brushdead=BM}}.

setcell(Etat,Cell) ->
	wx_object:call(?SERVER,{setcell,Etat,Cell}).

info(M) when is_atom(M) ->
	info(atom_to_list(M));
info(M) ->
	wx_object:cast(?SERVER,{info,M}).

refreshtoggle(true) ->
	wx_object:cast(?SERVER,refreshtoggle);
refreshtoggle(false) ->
	wx_object:cast(?SERVER,norefreshtoggle).

refresh() ->
	wx_object:call(?SERVER,refresh).

enable() ->
	wx_object:cast(?SERVER,enable).


%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{panel=Panel, memoryDC=MDC, w=W, h=H}) ->
    redraw(Panel,MDC,W,H).

handle_info(M,S = #state{frame=F}) -> 
    M1 = io_lib:format("Received unexpected message ~p",[M]),
    wxFrame:setStatusText(F,M1,[]),
    {noreply, S}.

handle_event(#wx{id=?MAIN,event=#wxClose{}}, S) ->
	lavie_server:reset(),
    {stop, shutdown, S};
handle_event(#wx{id=?CHILD,obj=O,event=#wxClose{}}, #state{frame=F} = S) ->
	wxFrame:destroy(O),
	enable(F),
	wxWindow:setFocus(F),
    {noreply, S};
handle_event(#wx{id=?WSTORE,obj=O,event = #wxFileDirPicker{type = command_filepicker_changed, path = Path}}, #state{frame=F} = S) ->
	lavie_fsm:save(Path),
	wxWindow:destroy(wxFilePickerCtrl:getParent(O)),
	enable(F),
    {noreply, S};
handle_event(#wx{id=?WREAD,obj=O,event = #wxFileDirPicker{type = command_filepicker_changed, path = Path}}, #state{frame=F} = S) ->
	lavie_fsm:read(Path),
	wxWindow:destroy(wxFilePickerCtrl:getParent(O)),
	enable(F),
    {noreply, S};
handle_event(#wx{event=#wxMouse{type=left_up,x=X,y=Y}},#state{w=W,h=H,zoom=Z}=S) ->
    lavie_server:click(min(X,W-2) div (Z+1), min(Y,H-2) div (Z+1)),
    {noreply,S};
handle_event(#wx{event=#wxMouse{type=middle_down}},S) ->
    lavie_fsm:middle_down(),
    {noreply,S};
handle_event(#wx{event=#wxMouse{type=right_down}},S) ->
    lavie_fsm:right_down(),
    {noreply,S};
handle_event(#wx{event=#wxCommand{type=command_button_clicked},id=Id},#state{frame=F} = S) ->
    M = keypress(Id,F),
	wxFrame:setStatusText(F,M,[]),
    {noreply,S};
handle_event(E, #state{frame=F}= S) ->
    M = io_lib:format("ignore event ~p ",[E#wx.event]),
    io:format("ignore event ~p~n",[E]),
    wxFrame:setStatusText(F,M,[]),
    {noreply,S}.

handle_call(refresh, _From, #state{cellbuf=Cb, memoryDC=MDC, zoom=Z, penlive=PV, pendead=PM, brushlive=BV, brushdead=BM,panel=P} = State) ->
	do_refresh(Cb,Z,PV,PM,BV,BM,P,MDC),
    {reply, ok, State#state{cellbuf=[]}};
handle_call({setcell,Etat,Cell}, _From, #state{cellbuf=Cb, memoryDC=MDC, zoom=Z, penlive=PV, pendead=PM, brushlive=BV, brushdead=BM,panel=P,refreshtoggle=true} = State) ->
	do_refresh([{Etat,Cell}|Cb],Z,PV,PM,BV,BM,P,MDC),
    {reply, ok, State#state{cellbuf=[]}};
handle_call({setcell,Etat,Cell}, _From, #state{cellbuf=Cb} = State) ->
    {reply, ok, State#state{cellbuf=[{Etat,Cell}|Cb]}};
handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

handle_cast({info,M}, #state{frame=F} = State) ->
	wxFrame:setStatusText(F,M,[]),
    {noreply, State};
handle_cast(refreshtoggle, State) ->
    {noreply, State#state{refreshtoggle=true}};
handle_cast(norefreshtoggle, State) ->
    {noreply, State#state{refreshtoggle=false}};
handle_cast(enable, #state{frame=F} = State) ->
	wxWindow:enable(F),
	wxWindow:setFocus(F),
    {noreply, State};
handle_cast(_What, State) ->
    {noreply, State}.
    
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(Reason, _State) ->
    io:format("terminate Lavie gui with reason = ~p~n",[Reason]),
    application:stop(lavie),
    normal.

%%%%%%%%%%%%%%%%%%%%% local functions %%%%%%%%%%%%%

create_window(W1,H1,Z) ->
	Zt = Z + 1,
	W = Zt * W1 + 1,
	H = Zt * H1 + 1,
    Frame = wxFrame:new(wx:null(), ?MAIN, "Le jeu de la vie (C) Pascal Chapier", 
    							[{size,{max(W + 24,270), H + 167}},
                                {style,	?wxMINIMIZE_BOX bor
                                 	?wxSYSTEM_MENU bor
                                 	?wxCAPTION  bor
                                 	?wxCLOSE_BOX
                                 }]),
    wxFrame:setStatusBar(Frame,wxFrame:createStatusBar(Frame,[])),
    wxFrame:connect(Frame, close_window), 
    fill_window(W,H,Frame,Z).

fill_window(W,H,Frame,Z) ->
   	MainSz = wxBoxSizer:new(?wxVERTICAL),
    Board = wxPanel:new(Frame),
    wxWindow:setSizer(Board,MainSz),

   	PanSz = wxStaticBoxSizer:new(?wxVERTICAL, Board,[{label, "Le Monde"}]),
    KeySz = wxStaticBoxSizer:new(?wxVERTICAL, Board,[{label, "Controle"}]),
    KeyGrSz = wxGridSizer:new(2,3,2,2),

	Panel = wxPanel:new(Board, [{size,{W,H}}]),  

    wxSizer:addSpacer(MainSz,2),
    wxSizer:add(PanSz, Panel, [{flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:add(MainSz, PanSz, [{proportion, 0}, {border, 4}, {flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:addSpacer(MainSz,3),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, left_up, []),
	wxPanel:connect(Panel, right_down, []),
	wxPanel:connect(Panel, middle_down, []),

    wxSizer:addSpacer(MainSz,3),
	wxSizer:add(KeySz, KeyGrSz, [{flag, ?wxALL bor ?wxEXPAND}]), 
	[wxSizer:add(KeyGrSz, wxButton:new(Board,Id,[{label,Txt},{size,{80,20}}]), [{flag, ?wxALL}]) || {Id,Txt} <- ?BLIST],
    wxSizer:add(MainSz, KeySz, [{proportion, 0}, {border, 4}, {flag, ?wxALL}]), 
    wxSizer:addSpacer(MainSz,2),
    wxWindow:connect(Board, command_button_clicked),
    wxSizer:layout(MainSz),

	Bitmap = wxBitmap:new(W,H),
	PM = wxPen:new(color(dead), [{width, 1}]),
	PV = wxPen:new(color(live), [{width, 1}]),
	BG 	= wxBrush:new(color(background)),
	BM 	= wxBrush:new(color(dead)),
	BV 	= wxBrush:new(color(live)),

%% initialisation de l'image de départ
	MemoryDC = wxMemoryDC:new(Bitmap),
	wxDC:setBackground(MemoryDC,BG),
	wxDC:setBrush(MemoryDC,BG),
	PenTemp = wxPen:new(color(background), [{width, 1}]),
	wxDC:setPen(MemoryDC,PenTemp),
	wxDC:drawRectangle(MemoryDC, {0,0}, {W,H}),
	wxPen:destroy(PenTemp),
	wxDC:setPen(MemoryDC,PM),
	wxDC:setBrush(MemoryDC,BM),
    [cell(MemoryDC, {X,Y},Z) || X <- lists:seq(0,W-1), Y <- lists:seq(0,H-1)],
    redraw(Panel,MemoryDC,W,H),

    wxWindow:refresh(Panel,[{eraseBackground, false}]),
    wxWindow:show(Frame),
    {Frame, Panel, Bitmap, MemoryDC, PV, PM, BV, BM}.

color(live) -> {255,255,255};
color(dead) -> {80,80,80};
color(background) -> {0,0,40}.

redraw(Panel, MemoryDC, W, H) ->
    DC = wxPaintDC:new(Panel),  
    wxDC:blit(DC,{0,0},{W,H},MemoryDC,{0,0}),
    wxPaintDC:destroy(DC).

keypress(?FAST,_F) ->
	lavie_fsm:faster(),
	"accelère";
keypress(?SLOW,_F) ->
	lavie_fsm:slower(),
	"ralenti";
keypress(?RUN,_F) ->
    lavie_fsm:dclick(),
    "marche/arret";
keypress(?STORE,F) ->
	wxWindow:disable(F),
	accesfichier("enregistrer",?WSTORE, ?wxFLP_SAVE bor ?wxFLP_OVERWRITE_PROMPT),
    "enregistrer";
keypress(?READ,F) ->
	wxWindow:disable(F),
	accesfichier("lire",?WREAD, ?wxFLP_OPEN bor ?wxFLP_FILE_MUST_EXIST),
    "lire";
keypress(?RULE,F) ->
	wxWindow:disable(F),
	changerule(),
    "Changement des regles";
keypress(_,_F) ->
    "pas defini".

accesfichier(Titre,Id,Style) ->
    T = wxFrame:new(wx:null(), ?CHILD, Titre, 
    							[{size,{400,60}},
                                {style,	
                                 	?wxSYSTEM_MENU bor
                                 	?wxCAPTION  bor
                                 	?wxCLOSE_BOX
                                 }]),
    wxFrame:connect(T, close_window), 
    FilePicker = wxFilePickerCtrl:new(T, Id, [{style , 
    											?wxFLP_USE_TEXTCTRL bor Style
    											}]),
    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    wxWindow:show(T).

changerule() ->
	rule_wx:start_link().

enable(F) ->
	wxWindow:enable(F),
	wxWindow:setFocus(F).

do_refresh(Cb,Z,PV,PM,BV,BM,P,MemoryDC) ->
	wx:batch(fun ()  ->
		Cb1 = lists:reverse(Cb),
		[cell(MemoryDC,PV,BV,PM,BM,State,Cell,Z) || {State,Cell} <- Cb1],
	    wxWindow:refresh(P,[{eraseBackground,false}])
	end).

cell(MemoryDC,_PV,_BV,PM,BM,dead,{Orx,Ory},Z) ->
	wxDC:setPen(MemoryDC,PM),
	wxDC:setBrush(MemoryDC,BM),
	wxDC:drawRectangle(MemoryDC, {(Z+1)*Orx+1,(Z+1)*Ory+1}, {Z,Z});
cell(MemoryDC,PV,BV,_PM,_BM,live,{Orx,Ory},Z) ->
	wxDC:setPen(MemoryDC,PV),
	wxDC:setBrush(MemoryDC,BV),
	wxDC:drawRectangle(MemoryDC, {(Z+1)*Orx+1,(Z+1)*Ory+1}, {Z,Z}).

cell(MemoryDC, {Orx,Ory},Z) ->
	wxDC:drawRectangle(MemoryDC, {(Z+1)*Orx+1,(Z+1)*Ory+1}, {Z,Z}).

