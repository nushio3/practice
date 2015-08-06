#!/usr/bin/env python

import argparse
import math
import sys
import time
import six

import chainer
from chainer import cuda
import chainer.functions as F
from chainer import optimizers
import datetime
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
import random
import re
import time
import urllib2

parser = argparse.ArgumentParser()
parser.add_argument('--gpu', '-g', default=-1, type=int,
                    help='GPU ID (negative value indicates CPU)')
args = parser.parse_args()
mod = cuda if args.gpu >= 0 else np

n_epoch = 39     # number of epochs
n_units = 5     # number of units per layer
batchsize = 20  # minibatch size
bprop_len = 1   # length of truncated BPTT
grad_clip = 5    # gradient norm threshold to clip
n_inputs = 1     # number of input time series
n_outputs = 1    # number of output time series



global lightcurve, time_begin, time_end
lightcurve = dict()

# todo: optimize this by use of sortedcontainers
def goes_future_max(t0, dt):
    t = t0
    ret = 0
    while t<=t0+dt:
        t += datetime.timedelta(minutes=1)
        if lightcurve.has_key(t):
            ret=max(ret, lightcurve[t])
    return ret

# Prepare RNNLM model
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


def forward_one_step(x_data, state, train=True):
    if args.gpu >= 0:
        x_data = cuda.to_gpu(x_data)
    x = chainer.Variable(x_data, volatile=not train)
    h0 = model.embed(x)
    h1_in = model.l1_x(F.dropout(h0, train=train)) + model.l1_h(state['h1'])
    c1, h1 = F.lstm(state['c1'], h1_in)

    print c1.data
    print h1.data

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

# Setup optimizer
optimizer = optimizers.SGD(lr=0.1)
optimizer.setup(model.collect_parameters())




while True:
    # time_begin = datetime.datetime(2014,3,8) + datetime.timedelta(hours=d)
    d = random.randrange(365*5*24)
    time_begin = datetime.datetime(2011,1,1) +  datetime.timedelta(hours=d)
    window_days = 10
    time_end   = time_begin + datetime.timedelta(days=window_days)

    t=time_begin
    while t <= time_end:
        # use random instead of actual data
        lightcurve[t] = 2.5*random.random()
        t+=datetime.timedelta(minutes=1)


    t_data = []
    goes_flux = []
    t=time_begin
    while t <= time_end:
        t_data.append(t)
        goes_flux.append(lightcurve[t])
        t+=datetime.timedelta(minutes=1)

    goes_max = []
    for t in t_data:
        goes_max.append(goes_future_max(t,datetime.timedelta(hours=1)))

    whole_len = len(t_data)
    jump = whole_len // batchsize
    cur_log_perp = mod.zeros(())
    epoch = 0
    start_at = time.time()
    cur_at = start_at
    state = make_initial_state()
    accum_loss = chainer.Variable(mod.zeros((), dtype=np.float32))
    print('going to train {} iterations'.format(jump * n_epoch))

    for i in range(2): #six.moves.range(jump*n_epoch):
        print i
        x_batch = np.array([[goes_flux[(jump * j + i) % whole_len]]
                        for j in six.moves.range(batchsize)], dtype=np.float32)
        truth_data = np.array([[goes_max[(jump * j + i) % whole_len]]
                        for j in six.moves.range(batchsize)], dtype=np.float32)
        if args.gpu >= 0:
            truth_data = cuda.to_gpu(y_batch)
        truth = chainer.Variable(truth_data, volatile=False)

        state, y = forward_one_step(x_batch, state)


        loss_i = F.mean_squared_error(y,truth)
        accum_loss += loss_i
        cur_log_perp += loss_i.data.reshape(())

        optimizer.clip_grads(grad_clip)
        optimizer.update()

        if (i + 1) % bprop_len == 0:  # Run truncated BPTT
            print i, jump*n_epoch
            optimizer.zero_grads()
            print accum_loss.data
            accum_loss.backward()
            accum_loss.unchain_backward()  # truncate
            accum_loss = chainer.Variable(mod.zeros((), dtype=np.float32))


            optimizer.clip_grads(grad_clip)
            optimizer.update()
    exit(0)
