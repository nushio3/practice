reset

set xrange [0:80]
set yrange [0:30]



set pm3d
set pm3d map
set multiplot

splot 'tbl.txt' t ''

unset pm3d

splot 'points.txt' u 1:2:(0) w p pt 7 lc rgb '#ffffff' t ''
