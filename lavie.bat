echo off
d:
cd "\git\lavie"
start erl -pa "./ebin" -s lavie -noshell -detach -width 300 -height 150