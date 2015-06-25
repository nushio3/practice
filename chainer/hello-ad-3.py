#!/usr/bin/env python
import argparse
import numpy as np
from chainer import cuda, Variable, FunctionSet, optimizers
import chainer.functions  as F

parser = argparse.ArgumentParser(description='Chainer Optimizer Test')
parser.add_argument('--optimizer', '-o', default='SGD',
                    help='Name of the optimizer function')
args = parser.parse_args()

N=20


def dist2(p1, p2):
    (x1,y1) = p1
    (x2,y2) = p2
    return (x1-x2)**2 + (y1-y2)**2

def forward(model):
    fx = model.lx
    fy = model.ly
    pos = [[]]*N
    lit0 = Variable(np.array([[0]*N], dtype=np.float32))
    for i in range(N):
        ptr_list = [0]*N
        ptr_list[i] = 1
        ptr = Variable(np.array([ptr_list], dtype=np.float32))
        x = fx(ptr) - fx(lit0)
        y = fy(ptr) - fy(lit0)
        pos[i] = (x,y)

    potential = 0
    for i in range(N):
        r2 = dist2(pos[i],(0,0))
        potential += 1e-2 ** r2
    for i in range(N):
        for j in range(i+1,N):
            r2 = dist2(pos[i],pos[j])
            potential += r2**(-6)-2*r2**(-3)

    return potential

model=()

while True:
    model =FunctionSet(
        lx = F.Linear(N,1,30),
        ly = F.Linear(N,1,30)
    )
    potential = forward(model).data[0][0]
    print 'candidate:{}'.format(potential)
    if (potential>-0.1 and potential<0) :
        break

optimizer = eval('optimizers.{}()'.format(args.optimizer))
optimizer.setup(model.collect_parameters())



# system('mkdir -p result')
log_filename = 'result/log-{}.txt'.format(args.optimizer)
# system('rm {}'.format(log_filename))

t = 0
while True:
    optimizer.zero_grads()
    potential = forward(model)
    potential.backward()
    optimizer.update()
    # print model.lx.W
    # print model.ly.W
    # print potential.data
    t+=1
    with open(log_filename, "a") as fp:
        fp.write('{} {}\n'.format(t,potential.data[0][0]))
    if (t%100==0 or t==1) :
        print '{} {} {}'.format(args.optimizer,t,potential.data[0][0])
        snapshot_filename = 'result/{}-{:06d}.txt'.format(args.optimizer, t)
        with open(snapshot_filename, "w") as fp:
            for i in range(N):
                fp.write('{} {}\n'.format(model.lx.W[0][i], model.ly.W[0][i]))
