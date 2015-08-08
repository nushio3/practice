#!/usr/bin/env python
"""
Test Chainer LSTM on very simple data.
"""
import argparse
import hashlib
import math
import sys
import subprocess
import time
import re
import numpy as np
import six

import chainer
from chainer import cuda
import chainer.functions as F
from chainer import optimizers

n_iter = 10000     # number of iterations
n_units = 650    # number of units per layer
batchsize = 1    # minibatch size
bprop_len = 2   # length of truncated BPTT
grad_clip = 500    # gradient norm threshold to clip
n_inputs = 1
n_outputs = 1

parser = argparse.ArgumentParser()
parser.add_argument('--gpu', '-g', default=-1, type=int,
                    help='GPU ID (negative value indicates CPU)')
parser.add_argument('--optimizer', '-o', default='AdaGrad',
                    help='Name of the optimizer function')
parser.add_argument('--optimizeroptions', '-p', default='()',
                    help='Tuple of options to the optimizer')
parser.add_argument('--filename', '-f', default='',
                    help='Experiment filename tag')

args = parser.parse_args()
mod = cuda if args.gpu >= 0 else np

print args

filename_base = args.filename
if filename_base=='':
    filename_base = args.optimizer + "_" + hashlib.md5(str(args)).hexdigest()
subprocess.call("mkdir result -p", shell=True)
config_filename = 'result/{}_config.txt'.format(filename_base)
log_filename = 'result/{}_current.txt'.format(filename_base)
prediction_filename = 'result/{}_prediction.txt'.format(filename_base)
for fn in [log_filename, prediction_filename]:
    subprocess.call("rm '{}'".format(fn), shell=True)
    print fn
with open(config_filename, "w") as fp:
    fp.write(str(args) + '\n')

model = chainer.FunctionSet(embed=F.Linear(n_inputs, n_units),
                            l1_x=F.Linear(n_units, 4 * n_units),
                            l1_h=F.Linear(n_units, 4 * n_units),
                            l2_x=F.Linear(n_units, 4 * n_units),
                            l2_h=F.Linear(n_units, 4 * n_units),
                            l3=F.Linear(n_units, n_outputs))
for param in model.parameters:
    param[:] = np.random.uniform(-0.1, 0.1, param.shape)
if args.gpu >= 0:
    cuda.init(args.gpu)
    model.to_gpu()

# Setup optimizer
optimizer_expr = 'optimizers.{}{}'.format(args.optimizer, args.optimizeroptions)
optimizer = eval(optimizer_expr)
optimizer.setup(model.collect_parameters())

def forward_one_step(x, state, train=True):
    drop_ratio = 0.5
    h0 = model.embed(x)
    h1_in = model.l1_x(F.dropout(h0,ratio=drop_ratio, train=train)) + model.l1_h(state['h1'])
    c1, h1 = F.lstm(state['c1'], h1_in)

    h2_in = model.l2_x(F.dropout(h1,ratio=drop_ratio, train=train)) + model.l2_h(state['h2'])
    c2, h2 = F.lstm(state['c2'], h2_in)


    y = model.l3(F.dropout(h2,ratio=drop_ratio, train=train))
    state = {'c1': c1, 'h1': h1, 'c2': c2, 'h2': h2}
    return state, y


def make_initial_state(batchsize=batchsize, train=True):
    return {name: chainer.Variable(mod.zeros((batchsize, n_units),
                                             dtype=np.float32),
                                   volatile=not train)
            for name in ('c1', 'h1', 'c2', 'h2')}

def curve(t):
    #return math.sin(t) + 2*math.sin(math.sqrt(3)*t) + 3*math.sin(math.sqrt(10)*t)
    o2= 20 * math.pi
    a2 = 2
    if t > o2:
        a2 = 2 + 4*math.sin((t-o2)/200)
    return math.sin(t) + a2*math.sin(2*t) + 3*math.sin(3*t)



t=0
state = make_initial_state()
state_test = make_initial_state(train=False)
dt = 0.1
predict_dt = 1.0
sum_error = 0.0
for i0 in range(n_iter):
    accum_loss = chainer.Variable(mod.zeros((), dtype=np.float32))

    t+=dt
    x = curve(t)
    x_data = [x]
    future_y = curve(t+predict_dt)
    y_data = [future_y]
    x_batch = np.array([x_data], dtype=np.float32)
    y_batch = np.array([y_data], dtype=np.float32)
    x = chainer.Variable(x_batch)
    x_volatile = chainer.Variable(x_batch,volatile=True)

    y_truth = chainer.Variable(y_batch)

    state, y_pred = forward_one_step(x, state)
    loss_i = F.mean_squared_error(y_pred, y_truth)
    accum_loss += loss_i

    state_test, y_test = forward_one_step(x_volatile, state_test,train=False)

    with open(log_filename,'a') as fp:
        msg = '{} {} {} {} {}'.format(t, x.data[0,0], y_truth.data[0,0], y_pred.data[0,0], y_test.data[0,0])
        print msg
        fp.write(msg+'\n')

    """
    At this moment, neural network knows the information upto (t+predict_dt)
    via teacher signal. This means that, observational data upto (t+predict_dt)
    is available to us. If you really want to say that the NN can predict the future
    at this moment, you need to predict curve(t+2*predict_dt) from now.
    """
    t2 = t
    state2 = state_test
    y2 = None
    while t2 < t+predict_dt:
        t2+=dt
        x_data = [curve(t2)]
        x2 = np.array([x_data], dtype=np.float32)
        x2 = chainer.Variable(x2,volatile=True)
        state2,y2 = forward_one_step(x2, state2, train=False)
    with open(prediction_filename,'a') as fp:
        msg = '{} {}'.format(t2+predict_dt, y2.data[0,0])
        fp.write(msg+'\n')
    sum_error += (curve(t2+predict_dt) - y2.data[0,0])**2

    if ((i0+1) % bprop_len == 0):
        optimizer.zero_grads()
        accum_loss.backward()
        accum_loss.unchain_backward()
        optimizer.clip_grads(grad_clip)
        optimizer.update()

with open(config_filename, "a") as fp:
    fp.write("mean prediction error : {}\n".format(sum_error / n_iter))
