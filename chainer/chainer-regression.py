#!/usr/bin/env python
"""Chainer example: train a multi-layer perceptron on diabetes dataset

This is a minimal example to write a feed-forward net. It requires scikit-learn
to load diabetes dataset.

Original program written by https://gist.github.com/mottodora/a9c46754cf555a68edb7/download#

"""
import argparse
import numpy as np
from sklearn.datasets import load_diabetes
from chainer import cuda, Variable, FunctionSet, optimizers
import chainer.functions  as F


parser = argparse.ArgumentParser(description='Chainer example: MNIST')
parser.add_argument('--gpu', '-g', default=-1, type=int,
                    help='GPU ID (negative value indicates CPU)')
args = parser.parse_args()

batchsize = 13
n_epoch   = 100
n_units   = 30

# Prepare dataset
print 'fetch diabetes dataset'
diabetes = load_diabetes()
data = diabetes['data'].astype(np.float32)
target = diabetes['target'].astype(np.float32).reshape(len(diabetes['target']), 1)

N = batchsize * 30
x_train, x_test = np.split(data, [N])
y_train, y_test = np.split(target, [N])
N_test = y_test.size

# Prepare multi-layer perceptron model
model = FunctionSet(l1=F.Linear(10, n_units),
                    l2=F.Linear(n_units, n_units),
                    l3=F.Linear(n_units, 1))
if args.gpu >= 0:
    cuda.init(args.gpu)
    model.to_gpu()

# Neural net architecture
def forward(x_data, y_data, train=True):
    x, t = Variable(x_data), Variable(y_data)
    h1 = F.dropout(F.relu(model.l1(x)),  train=train)
    h2 = F.dropout(F.relu(model.l2(h1)), train=train)
    y  = model.l3(h2)
    return F.mean_squared_error(y, t), y

# Setup optimizer
optimizer = optimizers.AdaDelta(rho=0.9)
optimizer.setup(model.collect_parameters())

# Learning loop
for epoch in xrange(1, n_epoch+1):
    print 'epoch', epoch

    # training
    perm = np.random.permutation(N)
    sum_loss = 0

    for i in xrange(0, N, batchsize):
        x_batch = x_train[perm[i:i+batchsize]]
        y_batch = y_train[perm[i:i+batchsize]]
        if args.gpu >= 0:
            x_batch = cuda.to_gpu(x_batch)
            y_batch = cuda.to_gpu(y_batch)

        optimizer.zero_grads()
        loss, pred = forward(x_batch, y_batch)
        loss.backward()
        optimizer.update()

        sum_loss += float(cuda.to_cpu(loss.data)) * batchsize

    print 'train mean loss={0}'.format(
        sum_loss / N)

    sum_loss     = 0
    preds = []
    for i in xrange(0, N_test, batchsize):
        x_batch = x_test[i:i+batchsize]
        y_batch = y_test[i:i+batchsize]
        if args.gpu >= 0:
            x_batch = cuda.to_gpu(x_batch)
            y_batch = cuda.to_gpu(y_batch)

        loss, pred = forward(x_batch, y_batch, train=False)
        preds.extend(cuda.to_cpu(pred.data))
        sum_loss     += float(cuda.to_cpu(loss.data)) * batchsize
    pearson = np.corrcoef(np.asarray(preds).reshape(len(preds),), np.asarray(y_test).reshape(len(preds),))


    print 'test  mean loss={0}, corrcoef={1}'.format(
        sum_loss / N_test, pearson[0][1])
