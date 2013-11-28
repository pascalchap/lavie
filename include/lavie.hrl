% id definition
-define(MAIN,200).
-define(CHILD,201).
-define(WSTORE,202).
-define(WREAD,203).
-define(GRDLIVE,204).
-define(GRDBORN,205).

-define(FAST,100).
-define(SLOW,101).
-define(RUN,102).
-define(STORE,103).
-define(READ,104).
-define(RULE,105).

%button definition
% main window
-define(B_FAST,{?FAST,"plus vite"}).
-define(B_SPARE,{-1,""}).
-define(B_SLOW,{?SLOW,"moins vite"}).
-define(B_RUN,{?RUN,"marche/arret"}).
-define(B_STORE,{?STORE,"enregistrer"}).
-define(B_READ,{?READ,"lire"}).
-define(B_RULE,{?RULE,"changer les regles"}).
-define(BLIST,[?B_FAST,?B_RUN,?B_SLOW,?B_STORE,?B_READ,?B_RULE]).

% rule window