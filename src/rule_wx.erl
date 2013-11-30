-module (rule_wx).

-export ([start_link/0]).

-include_lib("wx/include/wx.hrl").
-include("../include/lavie.hrl").

-behaviour(wx_object).

-record (state, {
            frame   % frame object
            ,grida  % contient la règle pour la survie
            ,gridb  % contient la règle pour la naissance
            }).

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
handle_event(#wx{event=#wxCommand{type=command_button_clicked},id=?CANCEL},#state{frame=F} = S) ->
    lavie_wx:enable(),
    wxFrame:destroy(F),
    {stop, normal, S};
handle_event(#wx{event=#wxCommand{type=command_button_clicked},id=?VALID},#state{frame=F,grida=Grida,gridb=Gridb} = S) ->
    Ra = getTable(Grida,9),
    Rb = getTable(Gridb,9),
%    io:format("rules ~p~n~p~n",[Ra,Rb]),
    lavie_server:setrule(Ra,Rb),
    lavie_wx:enable(),
    wxFrame:destroy(F),
    {noreply,S};
handle_event(_E,S) ->
%    io:format("unknown event ~p, state ~p~n",[E,S]),
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
    T = wxFrame:new(wx:null(), ?CHILD, "Edition des regles", 
    							[{size,{300,500}},
                                {style,	
                                 	?wxSYSTEM_MENU bor
                                 	?wxCAPTION  bor
                                 	?wxCLOSE_BOX
                                 }]),

    Panel = wxPanel:new(T, []),
    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    SizerLive = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "regle pour la survie en fonction du nombre de voisins"}]),
    SizerBirdth = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "regle pour les naissances en fonction du nombre de voisins"}]),
    KeyGrSz = wxGridSizer:new(1,2,2,2),

    {value,{Rulea,Ruleb}} = lavie_server:getrule(),
    Grida = addgrid(Panel, ?GRDLIVE, Rulea,"Survie\npossible"),
    Gridb = addgrid(Panel, ?GRDBORN, Ruleb,"Naissance\npossible"),

    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],
    wxSizer:add(SizerLive, Grida, Options),
    wxSizer:add(SizerBirdth, Gridb, Options),
    wxSizer:add(MainSizer, SizerLive, [{flag, ?wxEXPAND}, {proportion, 16}]),
    wxSizer:addSpacer(MainSizer,5),
    wxSizer:add(MainSizer, SizerBirdth, [{flag, ?wxEXPAND}, {proportion, 16}]),
    wxSizer:addSpacer(MainSizer,5),
    [wxSizer:add(KeyGrSz, wxButton:new(Panel,Id,[{label,Txt},{size,{80,20}}]), [{flag, ?wxALL}]) || {Id,Txt} <- [?B_VALID,?B_CANCEL]],
    wxSizer:add(MainSizer, KeyGrSz, [{flag, ?wxEXPAND}, {proportion, 3}]), 
    wxWindow:connect(Panel, command_button_clicked),

    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:connect(T, close_window), 
    wxWindow:show(T),
    {T,#state{frame=T,grida=Grida,gridb=Gridb}}.

addgrid(P,Id,Rule,Title) ->
    %% Create the grid with 100 * 5 cells
    Grid = wxGrid:new(P, Id, []),
    wxGrid:createGrid(Grid, 9, 2),
    wxGrid:setDefaultCellAlignment(Grid,?wxALIGN_CENTER,?wxALIGN_CENTER),
    wxGrid:setColLabelValue(Grid, 0, Title),
    wxGrid:setColLabelValue(Grid, 1, "Probabilite"),
    wxGrid:setColFormatBool(Grid,0),
    io:format("addgrid ~p~n",[Rule]),
    %% Fun to set the values and flags of the cells
    Fun =
    fun(Row) ->
        wxGrid:setRowLabelValue(Grid, Row, io_lib:format("~p",[Row])),
        Prob = proplists:get_value(Row,Rule,0),
        case Prob of
            0 -> 
                wxGrid:setCellValue(Grid, Row, 0, "0"),
                wxGrid:setCellValue(Grid, Row, 1, "0");
            Prob ->
                wxGrid:setCellValue(Grid, Row, 0, "1"),
                wxGrid:setCellValue(Grid, Row, 1, io_lib:format("~p",[Prob]))
            end
    end,

    %% Apply the fun to each row
    wx:foreach(Fun, lists:seq(0,8)),
    wxGrid:setColSize(Grid, 2, 150),
    wxGrid:connect(Grid, grid_cell_change),
    Grid.

getTable(G,R) ->
    getTable(G,R,[]).

getTable(_,0,Res) -> Res;
getTable(G,R,Res) -> getTable(G,R-1,getRow(G,R-1,Res)).

getRow(G,R,Res) ->
    Used = wxGrid:getCellValue(G,R,0),
    P = to_float(wxGrid:getCellValue(G,R,1)),
    case Used of
        "1" -> [{R,P}|Res];
        "0" -> Res;
        _ -> error
    end.

to_float("0") -> 0.0;
to_float("1") -> 1.0;
to_float(S) -> list_to_float(S).