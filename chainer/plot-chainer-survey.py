#!/usr/bin/env python

import subprocess

def system(cmd):
    subprocess.call(cmd, shell=True)


with open("tmp.gnu", "w") as fp:
    fp.write(""" 
set term postscript enhanced eps solid 20
set out "optimization-progress.eps"
set grid
set yrange  [-40:0]
plot "result/log-Adam-0.txt" w l
""")

system("gnuplot tmp.gnu")


