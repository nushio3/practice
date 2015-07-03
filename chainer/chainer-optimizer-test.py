#!/usr/bin/env python
import argparse
import subprocess
import numpy as np
from chainer import cuda, Variable, FunctionSet, optimizers
import chainer.functions  as F
import annealing, cma, random
import sys

parser = argparse.ArgumentParser(description='Chainer Optimizer Test')
parser.add_argument('--optimizer', '-o', default='SGD',
                    help='Name of the optimizer function')
parser.add_argument('--optimizer2', '-2', default='AdaGrad',
                    help='Name of the dual optimizer function')
parser.add_argument('--instance', '-i', default='',
                    help='Experiment instance')
parser.add_argument('--popsize', '-p', default='',
                    help='Population size (for those optimizers that supports)')
global args,popsize
args = parser.parse_args()

if args.popsize != '':
    popsize = int(args.popsize)
else:
    if args.optimizer=='cmaes':
        popsize = 15
    else:
        popsize = 10


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
    global t,args,popsize
    pos = [[]]*N
    for i in range(N):
        pos[i] = (vs[i], vs[N+i])
    ret = potential_function(pos)
    if (t%popsize==0) :
        log_filename = 'result/log-{}{}-{}.txt'.format(args.optimizer,popsize,args.instance)
        with open(log_filename, "a") as fp:
            fp.write('{} {}\n'.format(t/popsize,ret))
    if (t%(100*popsize)==0) :
        # print '{} {} {}'.format(args.optimizer,t,ret)
        snapshot_filename = 'result/{}{}-{}-{:06d}.txt'.format(args.optimizer,popsize,args.instance, t/popsize)
        with open(snapshot_filename, "w") as fp:
            for i in range(N):
                fp.write('{} {}\n'.format(vs[i],vs[N+i]))
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
    cma.fmin(forward_plain,init_state, 1, popsize=50)
    sys.exit()

if args.optimizer=='annealing' :
    init_state=range(2*N)
    for i in range(2*N):
        init_state[i] = 60.0*random.random()-30.0
    annealing.annealing(forward_plain,init_state,popsize)
    sys.exit()


model=()

while True:
    model =FunctionSet(
        lx = F.Linear(N,1,30),
        ly = F.Linear(N,1,30)
    )
    potential = forward(model).data[0][0]
    print 'candidate:{}'.format(potential)
    if (potential>0 and potential<1) :
        break

optimizer = eval('optimizers.{}()'.format(args.optimizer))
optimizer.setup(model.collect_parameters())
optimizer2 = False
if args.optimizer2 != '':
    optimizer2 = eval('optimizers.{}()'.format(args.optimizer2))
    optimizer2.setup(model.collect_parameters())


optimizer_switch = False

for t in range(0,60000):
    if optimizer2==False:
        optimizer.zero_grads()
        potential = forward(model)
        potential.backward()
        optimizer.update()
    else:
        if optimizer_switch:
            optimizer.zero_grads()
            potential = forward(model)
            potential.backward()
            optimizer.update()
        else:
            optimizer2.zero_grads()
            potential = forward(model)
            potential.backward()
            optimizer2.update()
        if random.random()<0.003:
            optimizer_switch=not optimizer_switch
            print optimizer_switch

            # reset the optimizer
            optimizer = eval('optimizers.{}()'.format(args.optimizer))
            optimizer2 = eval('optimizers.{}()'.format(args.optimizer2))
            optimizer.setup(model.collect_parameters())
            optimizer2.setup(model.collect_parameters())

    # print model.lx.W
    # print model.ly.W
    # print potential.data

    optTag = args.optimizer+ args.optimizer2

    with open(log_filename, "a") as fp:
        fp.write('{} {}\n'.format(t,potential.data[0][0]))
    if (t%100==0) :
        print '{} {} {}'.format(optTag,t,potential.data[0][0])
        snapshot_filename = 'result/{}-{}-{:06d}.txt'.format(optTag,args.instance, t)
        with open(snapshot_filename, "w") as fp:
            for i in range(N):
                fp.write('{} {}\n'.format(model.lx.W[0][i], model.ly.W[0][i]))
