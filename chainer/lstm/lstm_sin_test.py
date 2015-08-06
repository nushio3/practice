#!/usr/bin/env python
"""
Test Chainer LSTM on very simple data.
"""
import argparse
import math
import sys
import subprocess
import time

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
grad_clip = 5    # gradient norm threshold to clip
n_inputs = 1
n_outputs = 1
log_filename = 'lstm_sin_log.txt'

parser = argparse.ArgumentParser()
parser.add_argument('--gpu', '-g', default=-1, type=int,
                    help='GPU ID (negative value indicates CPU)')
args = parser.parse_args()
mod = cuda if args.gpu >= 0 else np

subprocess.call('rm '+log_filename, shell=True)

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
optimizer = optimizers.SGD(lr=1.)
optimizer.setup(model.collect_parameters())


def forward_one_step(x_data, state, train=True):
    if args.gpu >= 0:
        x_data = cuda.to_gpu(x_data)
    x = chainer.Variable(x_data, volatile=not train)
    h0 = model.embed(x)
    h1_in = model.l1_x(F.dropout(h0, train=train)) + model.l1_h(state['h1'])
    c1, h1 = F.lstm(state['c1'], h1_in)
    h2_in = model.l2_x(F.dropout(h1, train=train)) + model.l2_h(state['h2'])
    c2, h2 = F.lstm(state['c2'], h2_in)
    y = model.l3(F.dropout(h2, train=train))
    state = {'c1': c1, 'h1': h1, 'c2': c2, 'h2': h2}
    return state, y


def make_initial_state(batchsize=batchsize, train=True):
    return {name: chainer.Variable(mod.zeros((batchsize, n_units),
                                             dtype=np.float32),
                                   volatile=not train)
            for name in ('c1', 'h1', 'c2', 'h2')}


t=0
state = make_initial_state()
for i0 in range(n_epoch * bprop_len):
    accum_loss = chainer.Variable(mod.zeros((), dtype=np.float32))

    t+=0.1
    x_data = [0.0]
    y = math.sin(t) + 2*math.sin(math.sqrt(0.5)*t) + 3*math.sin(t/math.sqrt(3))
    y_data = [y]
    x_batch = np.array([x_data], dtype=np.float32)
    y_batch = np.array([y_data], dtype=np.float32)

    y_truth = chainer.Variable(y_batch)

    state, y_pred = forward_one_step(x_batch, state)
    loss_i = F.mean_squared_error(y_pred, y_truth)
    accum_loss += loss_i

    with open(log_filename,'a') as fp:
        msg = '{} {} {}'.format(t, y_truth.data[0,0], y_pred.data[0,0])
        print msg
        fp.write(msg+'\n')

    if ((i0+1) % bprop_len == 0):
        optimizer.zero_grads()
        accum_loss.backward()
        accum_loss.unchain_backward()
        optimizer.clip_grads(grad_clip)
        optimizer.update()
