set term postscript enhanced color solid 20
set out "year_freq.eps"
set log y
set grid
set key left top
plot "< cat clockfreq*.dat" t '' pt 6
set out "year_flops.eps"
plot \
  "< ./preprocess.hs cpuflops.csv" t 'CPU' w lp pt 6 lw 2 \
, "< ./preprocess.hs gpuflops.csv" t 'GPU' w lp pt 6 lw 2