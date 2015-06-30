#!/usr/bin/env python

import subprocess

def system(cmd):
    subprocess.call(cmd, shell=True)


opts = []

for o in "AdaDelta AdaGrad Adam MomentumSGD RMSprop SGD".split():
    opts.append('-o {}'.format(o))

for i in [15,50,400]:
    opts.append('-o cmaes -p {}'.format(i))


opts2 = []
for i in range(15):
    for o in opts:
        opts2.append('./chainer-optimizer-test.py {} -i {}\n'.format(o,i))

with open("test-optimizers.sh", "w") as fp:
    fp.write("".join(opts2))

system("cat test-optimizers.sh | xargs -n 1 -P 12 python")
