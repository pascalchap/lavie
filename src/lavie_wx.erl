-module (lavie_wx).
%%
%% Include files
%%

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-define (SERVER , ?MODULE).
-define(MAIN,200).
-define(CHILD,201).
-define(WSTORE,202).
-define(WREAD,203).
-define(FAST,100).
-define(SLOW,101).
-define(RUN,102).
-define(STORE,103).
-define(READ,104).
-define(B_FAST,{?FAST,"plus vite"}).
-define(B_SPARE,{-1,""}).
-define(B_SLOW,{?SLOW,"moins vite"}).
-define(B_RUN,{?RUN,"marche/arret"}).
-define(B_STORE,{?STORE,"enregistrer"}).
-define(B_READ,{?READ,"lire"}).
-define(BLIST,[?B_FAST,?B_RUN,?B_SLOW,?B_STORE,?B_READ,?B_SPARE]).

-record(state,{frame  %% la fentre principale
			,panel    %% la zonne d'affichage des cellule
			,clientDC %% device contexte de panel
			,bitmap   %% le contenu a afficher
			,w        %% largeur
			,h        %% hauteur
			,penlive  %% crayon pour dessiner les cellules vivantes
			,pendead  %% crayon pour dessiner les cellules mortes
			}).

%%
%% Exported Functions
%%
-export([start_link/0
		,start_link/2
		,setcell/2
		,info/1
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
	start_link(200,50).
start_link(W,H) ->
    wx:new(),
%%    wx:debug(driver),
    {wx_ref,_Id,_WxType,Pid} = wx_object:start_link(?MODULE, [W,H], []),
    register(?SERVER,Pid),
    {ok,Pid}.

init([W,H]) ->
    {Frame, Panel, Bitmap, CDC, PV, PM} = wx:batch(fun() -> create_window(W,H) end),
    {Frame, #state{frame=Frame, panel=Panel, bitmap=Bitmap, clientDC=CDC, w=3*W+1, h=3*H+1, penlive=PV, pendead=PM}}.

setcell(Etat,Cell) ->
	wx_object:call(?SERVER,{setcell,Etat,Cell}).

info(M) when is_atom(M) ->
	info(atom_to_list(M));
info(M) ->
	wx_object:cast(?SERVER,{info,M}).


%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{panel=Panel, bitmap=Bitmap, w=W, h=H}) ->
    DC = wxPaintDC:new(Panel),	
    redraw(DC, Bitmap, W, H),
    wxPaintDC:destroy(DC),
    ok.

handle_info(M,S = #state{frame=F}) -> 
    M1 = io_lib:format("Received unexpected message ~p",[M]),
    wxFrame:setStatusText(F,M1,[]),
    {noreply, S}.

handle_event(#wx{id=?MAIN,event=#wxClose{}}, S) ->
    {stop, shutdown, S};
handle_event(#wx{id=?CHILD,obj=O,event=#wxClose{}}, S) ->
	wxFrame:destroy(O),
    {noreply, S};
handle_event(#wx{id=?WSTORE,obj=O,event = #wxFileDirPicker{type = command_filepicker_changed, path = Path}}, State) ->
	lavie_fsm:save(Path),
	wxWindow:destroy(wxFilePickerCtrl:getParent(O)),
    {noreply, State};
handle_event(#wx{id=?WREAD,obj=O,event = #wxFileDirPicker{type = command_filepicker_changed, path = Path}}, State) ->
	lavie_fsm:read(Path),
	wxWindow:destroy(wxFilePickerCtrl:getParent(O)),
    {noreply, State};
handle_event(#wx{event=#wxMouse{type=left_up,x=X,y=Y}},S) ->
    lavie_server:click(X div 3, Y div 3),
    {noreply,S};
handle_event(#wx{event=#wxMouse{type=middle_down}},S) ->
    lavie_fsm:middle_down(),
    {noreply,S};
handle_event(#wx{event=#wxMouse{type=right_down}},S) ->
    lavie_fsm:right_down(),
    {noreply,S};
handle_event(#wx{event=#wxCommand{type=command_button_clicked},id=Id},#state{frame=F} = S) ->
    M = keypress(Id),
	wxFrame:setStatusText(F,M,[]),
    {noreply,S};
handle_event(E, #state{frame=F}= S) ->
    M = io_lib:format("ignore event ~p ",[E#wx.event]),
    io:format("ignore event ~p~n",[E]),
    wxFrame:setStatusText(F,M,[]),
    {noreply,S}.


handle_call({setcell,Etat,Cell},_From, #state{clientDC=ClientDC, bitmap=Bitmap, w=W, h=H, penlive=PV, pendead=PM} = State) ->
	Pen = case Etat of
		live -> PV;
		dead -> PM
	end,
	setcell(ClientDC, Pen, Cell, Bitmap, W, H),
    {reply, ok, State};
handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

handle_cast({info,M}, #state{frame=F} = State) ->
	wxFrame:setStatusText(F,M,[]),
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

create_window(W1,H1) ->
	W = 3 * W1 + 1,
	H = 3 * H1 + 1,
    Frame = wxFrame:new(wx:null(), ?MAIN, "Le jeu de la vie (C) Pascal Chapier", 
    							[{size,{max(W + 16,270), H + 167}},
                                {style,	?wxMINIMIZE_BOX bor
                                 	?wxSYSTEM_MENU bor
                                 	?wxCAPTION  bor
                                 	?wxCLOSE_BOX
                                 }]),
    wxFrame:setStatusBar(Frame,wxFrame:createStatusBar(Frame,[])),
    wxFrame:connect(Frame, close_window), 
    fill_window(W,H,Frame).

fill_window(W,H,Frame) ->
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

	ClientDC = wxClientDC:new(Panel),
	Bitmap = wxBitmap:new(W,H),
	PM = wxPen:new(color(dead), [{width, 1}]),
	PV = wxPen:new(color(live), [{width, 1}]),
	BG 	= wxBrush:new(color(background)),

%% initialisation de l'image de départ
	MemoryDC = wxMemoryDC:new(Bitmap),
	wxDC:setBackground(MemoryDC,BG),
	wxDC:setBrush(MemoryDC,BG),
	PenTemp = wxPen:new(color(background), [{width, 1}]),
	wxDC:setPen(MemoryDC,PenTemp),
	wxDC:drawRectangle(MemoryDC, {0,0}, {W,H}),
	wxPen:destroy(PenTemp),
	wxDC:setPen(MemoryDC,PM),
    [cell(MemoryDC, {X,Y}) || X <- lists:seq(0,W-1), Y <- lists:seq(0,H-1)],
    redraw(ClientDC,Bitmap,W,H),

    wxSizer:layout(MainSz),
    wxWindow:show(Frame),
    wxWindow:refresh(Panel),    
    {Frame, Panel, Bitmap, ClientDC, PV, PM}.

color(live) -> {255,255,255};
color(dead) -> {80,80,80};
color(background) -> {0,0,40}.

redraw(DC, Bitmap, W, H) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},{W,H},MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).

cell(DC,{Orx,Ory}) ->
	% wxDC:drawRectangle(DC, {3*Orx+1,3*Ory+1}, {2,2}).
	X = 3*Orx+1,
	Y = 3*Ory+1,
	wxDC:drawPoint(DC,{X,Y}),
	wxDC:drawPoint(DC,{X,Y+1}),
	wxDC:drawPoint(DC,{X+1,Y}),
	wxDC:drawPoint(DC,{X+1,Y+1}).

setcell(DC,Pen,Cell,Bitmap,W,H) when is_list(Cell) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
	wxDC:setPen(MemoryDC,Pen),
    [cell(MemoryDC,Pos) || Pos <- Cell],
    wxDC:blit(DC, {0,0},{W,H},MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC);	    
setcell(DC,Pen,Cell,Bitmap,W,H) ->
    setcell(DC,Pen,[Cell],Bitmap,W,H).

keypress(?FAST) ->
	lavie_fsm:faster(),
	"accelère";
keypress(?SLOW) ->
	lavie_fsm:slower(),
	"ralenti";
keypress(?RUN) ->
    lavie_fsm:dclick(),
    "marche/arret";
keypress(?STORE) ->
	accesfichier("enregistrer",?WSTORE, ?wxFLP_SAVE bor ?wxFLP_OVERWRITE_PROMPT),
    "enregistrer";
keypress(?READ) ->
	accesfichier("lire",?WREAD, ?wxFLP_OPEN bor ?wxFLP_FILE_MUST_EXIST),
    "lire";
keypress(_) ->
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

