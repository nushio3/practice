#!/usr/bin/env python
import argparse
import subprocess
import numpy as np
from chainer import cuda, Variable, FunctionSet, optimizers
import chainer.functions  as F
import cma, random
import sys

parser = argparse.ArgumentParser(description='Chainer Optimizer Test')
parser.add_argument('--optimizer', '-o', default='SGD',
                    help='Name of the optimizer function')
parser.add_argument('--instance', '-i', default='',
                    help='Experiment instance')
global args
args = parser.parse_args()

N=20

def system(cmd):
    subprocess.call(cmd, shell=True)

def dist2(p1, p2):
    (x1,y1) = p1
    (x2,y2) = p2
    return (x1-x2)**2 + (y1-y2)**2

def potential_function(pos):
    potential = 0
    for i in range(N):
        r2 = dist2(pos[i],(0,0))
        potential += 1e-2 ** r2
    for i in range(N):
        for j in range(i+1,N):
            r2 = dist2(pos[i],pos[j])
            potential += r2**(-6)-2*r2**(-3)

    return potential

def forward_plain(vs):
    global t,args
    pos = [[]]*N
    for i in range(N):
        pos[i] = (vs[i], vs[N+i])
    ret = potential_function(pos)
#     if (t%10==0) :
#         with open(log_filename, "a") as fp:
#             fp.write('{} {}\n'.format(t,ret))
#     if (t%1000==0) :
#         # print '{} {} {}'.format(args.optimizer,t,ret)
#         snapshot_filename = 'result/{}-{}-{:06d}.txt'.format(args.optimizer,args.instance, t)
#         with open(snapshot_filename, "w") as fp:
#             for i in range(N):
#                 fp.write('{} {}\n'.format(vs[i],vs[N+i]))
    t=t+1
    return ret

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

    return potential_function(pos)

global t
t=0
system('mkdir -p result')
log_filename = 'result/log-{}-{}.txt'.format(args.optimizer,args.instance)
system('rm {}'.format(log_filename))



if args.optimizer=='cmaes' :
    init_state=range(2*N)
    for i in range(2*N):
        init_state[i] = 60.0*random.random()-30.0
    cma.fmin(forward_plain,init_state, 1)
    sys.exit()

model=()

while True:
    model =FunctionSet(
        lx = F.Linear(N,1,30),
        ly = F.Linear(N,1,30)
    )
    potential = forward(model).data[0][0]
    print 'candidate:{}'.format(potential)
    if (potential>0 and potential<0.1) :
        break

optimizer = eval('optimizers.{}()'.format(args.optimizer))
optimizer.setup(model.collect_parameters())




for t in range(0,60000):
    optimizer.zero_grads()
    potential = forward(model)
    potential.backward()
    optimizer.update()
    # print model.lx.W
    # print model.ly.W
    # print potential.data
    with open(log_filename, "a") as fp:
        fp.write('{} {}\n'.format(t,potential.data[0][0]))
    if (t%100==0) :
        print '{} {} {}'.format(args.optimizer,t,potential.data[0][0])
        snapshot_filename = 'result/{}-{}-{:06d}.txt'.format(args.optimizer,args.instance, t)
        with open(snapshot_filename, "w") as fp:
            for i in range(N):
                fp.write('{} {}\n'.format(model.lx.W[0][i], model.ly.W[0][i]))
