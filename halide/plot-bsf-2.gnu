set term postscript enhanced solid color 30

set grid
set log x



set ylabel 'performance (GFlop/s)'
set format x "10^{%T}"


set out 'figure/bsf_XO.eps'
set xlabel 'outer tile size X'
plot "< cat result/E_*.txt" u ($12):1 pt 7 t ''

set out 'figure/bsf_YO.eps'
set xlabel 'outer tile size Y'
plot "< cat result/E_*.txt" u ($13):1 pt 7 t ''

set out 'figure/bsf_XI.eps'
set xlabel 'inner tile size X'
plot "< cat result/E_*.txt" u ($14):1 pt 7 t ''

set out 'figure/bsf_YI.eps'
set xlabel 'inner tile size Y'
plot "< cat result/E_*.txt" u ($15):1 pt 7 t ''



set out 'figure/bsf_inner_area.eps'
set xlabel 'size of inner tile (X times Y)'
plot "< cat result/E_*.txt" u ($14*$15):1 pt 7 t ''

set out 'figure/bsf_outer_area.eps'
set xlabel 'size of outer tile (X times Y)'
plot "< cat result/E_*.txt" u ($12*$13):1 pt 7 t ''

set out 'figure/bsf_inner_aspect.eps'
set xlabel 'aspect ratio of inner tile (Y/X)'
plot "< cat result/E_*.txt" u ($15/$14):1 pt 7 t ''

set out 'figure/bsf_outer_aspect.eps'
set xlabel 'aspect ratio of outer tile (Y/X)'
plot "< cat result/E_*.txt" u ($13/$12):1 pt 7 t ''

set out 'figure/bsf_area_ratio.eps'
set xlabel 'ratio of the size of the outer/inner tile (XY/XY)'
plot "< cat result/E_*.txt" u (($12*$13)/($14*$15)):1 pt 7 t ''


set out 'figure/bsf_unroll.eps'
set xlabel 'unroll number'
plot "< cat result/E_*.txt" u 11:1 pt 7 t ''
