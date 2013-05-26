reset

set xrange [0:248]
set yrange [126:0]
set size ratio -1

set xlabel 'image X coordinate'
set ylabel 'image Y coordinate'
set cblabel 'likelihood'
set pm3d
set pm3d map
set multiplot

splot 'tbl.txt'  u 1:2:(tanh($3**4)) t ''

unset pm3d

splot 'points.txt' u 1:2:(0) w p pt 7 lc rgb '#ffffff' t ''
