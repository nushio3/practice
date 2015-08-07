#!/usr/bin/env python
"""
Test Chainer LSTM on very simple data.
"""
import argparse
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

n_epoch = 100     # number of epochs
n_units = 650    # number of units per layer
batchsize = 1    # minibatch size
bprop_len = 100   # length of truncated BPTT
grad_clip = 500    # gradient norm threshold to clip
n_inputs = 1
n_outputs = 1

parser = argparse.ArgumentParser()
parser.add_argument('--gpu', '-g', default=-1, type=int,
                    help='GPU ID (negative value indicates CPU)')
parser.add_argument('--optimizer', '-o', default='SGD',
                    help='Name of the optimizer function')
args = parser.parse_args()
mod = cuda if args.gpu >= 0 else np

log_filename = 'lstm_{}_current.txt'.format(args.optimizer)
prediction_filename = 'lstm_{}_prediction.txt'.format(args.optimizer)
for fn in [log_filename, prediction_filename]:
    subprocess.call("rm '{}'".format(fn), shell=True)
    print fn

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
optimizer_expr = args.optimizer
if not re.search('\(',optimizer_expr):
    optimizer_expr = 'optimizers.{}()'.format(optimizer_expr)
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
    # return math.sin(t) + 2*math.sin(math.sqrt(3)*t) + 3*math.sin(math.sqrt(10)*t)
    return math.sin(t) + 2*math.sin(2*t) + 3*math.sin(3*t)

t=0
state = make_initial_state()
state_test = make_initial_state(train=False)
last_y=0
dt = 0.1
predict_dt = 1.0
for i0 in range(n_epoch * bprop_len):
    accum_loss = chainer.Variable(mod.zeros((), dtype=np.float32))

    t+=dt
    y = curve(t)
    x_data = [y] #[last_y]
    future_y = curve(t+predict_dt)
    y_data = [future_y]
    last_y=y
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
        msg = '{} {} {} {} {}'.format(t, y, y_truth.data[0,0], y_pred.data[0,0], y_test.data[0,0])
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
        x2 = chainer.Variable(x_batch,volatile=True)
        state2,y2 = forward_one_step(x2, state2, train=False)
    with open(prediction_filename,'a') as fp:
        msg = '{} {}'.format(t2, y2.data[0,0])
        fp.write(msg+'\n')

    if t < 100:
        bprop_len_inner = 2
    else:
        bprop_len_inner = bprop_len

    if ((i0+1) % bprop_len_inner == 0):
        optimizer.zero_grads()
        accum_loss.backward()
        accum_loss.unchain_backward()
        optimizer.clip_grads(grad_clip)
        optimizer.update()
