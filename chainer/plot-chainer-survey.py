#!/usr/bin/env python

import subprocess

def system(cmd):
    subprocess.call(cmd, shell=True)

optimizers = "cmaes AdaDelta AdaGrad Adam MomentumSGD RMSprop SGD".split()

plots = []
color="#c0c0c0 #00ff00 #0000ff #00ffff #ff00ff #ffff00 #ff0000".split()

for o in range(7):
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
set xrange  [0:60000]
plot {plotsH}
""".format(plotsH = ','.join(plots)))

system("gnuplot tmp.gnu")
system("ps2pdf optimization-progress.eps")


