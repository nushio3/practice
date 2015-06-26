#!/usr/bin/env python

import subprocess

def system(cmd):
    subprocess.call(cmd, shell=True)

optimizers = "AdaDelta AdaGrad Adam MomentumSGD RMSprop SGD".split()

plots = []
color="#ff0000 #ff4040 #ff8080 #80ff00 #0000ff #00ff00".split()

for o in range(6):
    for i in range(6):
        plot = '"result/log-{}-{}.txt" w l lc rgb "{}" t ""'.format(optimizers[o],i,color[o])
        plots += [plot]

with open("tmp.gnu", "w") as fp:
    fp.write(""" 
set term postscript enhanced color eps solid 20
set out "optimization-progress.eps"
set grid
set yrange  [-40:0]
plot {plotsH}
""".format(plotsH = ','.join(plots)))

system("gnuplot tmp.gnu")


