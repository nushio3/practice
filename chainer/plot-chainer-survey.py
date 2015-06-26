#!/usr/bin/env python

import subprocess

def system(cmd):
    subprocess.call(cmd, shell=True)

optimizers = "AdaDelta AdaGrad Adam MomentumSGD RMSprop SGD".split()

plots = []
color="#80cc00 #00cc80 #00ff00 #ff0000 #0000ff #800000".split()

for o in range(6):
    for i in range(6):
        titlestr =  optimizers[o] if i==0 else ''
        plot = '"result/log-{}-{}.txt" w l lc rgb "{}" t "{}"'.format(optimizers[o],i,color[o],titlestr)
        plots += [plot]

with open("tmp.gnu", "w") as fp:
    fp.write(""" 
set term postscript enhanced color eps solid 20
set out "optimization-progress.eps"
set grid
set key bottom left
set yrange  [-50:0]
plot {plotsH}
""".format(plotsH = ','.join(plots)))

system("gnuplot tmp.gnu")


