set term postscript enhanced color 20
set out "freq_year.eps"
set grid
set log y
plot "< cat clockfreq*.dat" pt 6 t ""
