lavie
=====

simple implementation of the game of life

lancement de l'application avec une taille par defaut (200x50) ou parametree par les flags -width nombre et -height nombre
double click gauche pour demarrer/passer en standby
click gauche pour creer/tuer une cellule
click droit (en mode standby) pour faire du pas a pas
click milieu pour revenir en config puis tuer toutes les cellules

instalation
===========

cloner le depot
il faut disposer d'une installation de Erlang version superieure ou Ã©gale a la R15b et WxWidget
rebar compile
erl -pa "./ebin" -s lavie [-noshell] [-width integer] [-height integer]
