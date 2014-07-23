set term postscript enhanced landscape solid color 20

set grid
set out "figure/result-wct.eps"

set size ratio -1
set xlabel "problem size"
set ylabel "wall clock time"
set log xy
set key top left
set format x "10^{%L}"
set format y "10^{%L}"

plot "< awk '{if($3==1000)print $0}' result/result-1.txt"  u ($1*$2):($4)  t '1 core', \
      "< awk '{if($3==1000)print $0}' result/result-08-fine.txt"  u ($1*$2):($4)  t '64 core'

set out "figure/result-ef.eps"

set ylabel "update / time"

plot "< awk '{if($3==1000)print $0}' result/result-1.txt"  u ($1*$2):(($1*$2*$3)/$4)  t '1 core', \
      "< awk '{if($3==1000)print $0}' result/result-08-fine.txt"  u ($1*$2):(($1*$2*$3)/$4)  t '64 core'
