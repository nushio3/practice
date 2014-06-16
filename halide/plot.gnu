set term png  size 1024,768 


set pm3d
set pm3d map
set grid

set cbrange [0:1]

set palette model RGB defined ( 0 "white", 1 "red")


set size ratio -1

set out "debug.png"

splot "debug.txt" t ''

