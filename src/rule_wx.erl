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
    T = wxFrame:new(wx:null(), ?CHILD, "Edition des regles", 
    							[{size,{400,400}},
                                {style,	
                                 	?wxSYSTEM_MENU bor
                                 	?wxCAPTION  bor
                                 	?wxCLOSE_BOX
                                 }]),

    Panel = wxPanel:new(T, []),
    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "regle pour la survie en fonction du nombre de voisins"}]),

    {value,{Rulea,Ruleb}} = lavie_server:getrule(),
    Grid = addgrid(Panel,Rulea),

    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],
    wxSizer:add(Sizer, Grid, Options),
    wxSizer:add(MainSizer, Sizer, Options),
    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:connect(T, close_window), 
    wxWindow:show(T),
    {T,#state{frame=T}}.

addgrid(P,Rule) ->
    %% Create the grid with 100 * 5 cells
    Grid = wxGrid:new(P, ?GRDLIVE, []),
    wxGrid:createGrid(Grid, 9, 2),

    Font = wxFont:new(16, ?wxFONTFAMILY_SWISS,
              ?wxFONTSTYLE_NORMAL,
              ?wxFONTWEIGHT_NORMAL, []),
    wxGrid:setDefaultCellAlignment(Grid,?wxALIGN_CENTER,?wxALIGN_CENTER),
    wxGrid:setColLabelValue(Grid, 0, "Survie\npossible"),
    wxGrid:setColLabelValue(Grid, 1, "Probabilite"),
    wxGrid:setColFormatBool(Grid,0),
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
                wxGrid:setCellValue(Grid, Row, 0, "0"),
                wxGrid:setCellValue(Grid, Row, 1, io_lib:format("~p",[Prob]))
            end
    end,
    %% Apply the fun to each row
    wx:foreach(Fun, lists:seq(0,8)),
    wxGrid:setCellValue(Grid, 2, 0, "1"),
    wxGrid:setCellValue(Grid, 3, 0, "1"),
    wxGrid:setColSize(Grid, 2, 150),
    wxGrid:connect(Grid, grid_cell_change),
    Grid.

