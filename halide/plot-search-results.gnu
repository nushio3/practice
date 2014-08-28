set term postscript enhanced solid color 30

set grid
set log x



set ylabel 'performance (GFlop/s)'


set out 'figure/bench_vlen.eps'
set xlabel 'vector length'
plot "< cat result/C_*.txt" u 10:1 pt 7 t ''

set out 'figure/bench_fusion.eps'
set xlabel 'number of fused timesteps'
plot "< cat result/C_*.txt" u 7:1 pt 7 t ''

set out 'figure/bench_ulen.eps'
set xlabel 'unroll length'
plot "< cat result/C_*.txt" u 11:1 pt 7 t ''


set out 'figure/bench_timestep.eps'
set xlabel 'timestep'
plot "< cat result/C_*.txt" u 5:1 pt 7 t ''

set out 'figure/bench_space_size.eps'
set xlabel 'size of array (X times Y)'
plot "< cat result/C_*.txt" u ($3*$4):1 pt 7 t ''