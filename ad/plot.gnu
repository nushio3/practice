set term png size 1024,768 20
set yrange [-40:80]
set ylabel "Instruction clock addrs"
set xlabel "Iteration"
set grid
set out 'history-30.png'
plot 'history-30.txt' w d

reset

set term postscript enhanced color landscape 20
set xlabel "Problem Size"
set key top left
set ylabel "WCT for optimization (second)"
set grid
set out 'benchmark.eps'
plot 'benchmark.txt'

set xlabel "log_2(Problem Size)"
set key top left
set ylabel "log_2(WCT)"
set out 'benchmark-loglog.eps'
plot 'benchmark.txt' u (log($1) / log(2)):(log($2) / log(2)) t ''
