#!/usr/bin/env python
import numpy as np
from chainer import cuda, Variable, FunctionSet, optimizers
import chainer.functions  as F


lit0_data = np.array([0], dtype=np.float32)
lit0 = Variable(lit0_data)
lit1_data = np.array([1], dtype=np.float32)
lit1 = Variable(lit1_data)


model=FunctionSet(l1 = F.Linear(1,1))

# f = lambda (x) (W*x+b)
f = model.l1

print f.W
print f.b

b = f(lit0)
w = f(lit1) - f(lit0)

y = w**2 + 4*w + 4 + b**2 -6*b+9

print y.data

f.gW.fill(0)
f.gb.fill(0)

y.grad = np.ones((1, 1), dtype=np.float32)
y.backward()

print f.gW
print f.gb


optimizer = optimizers.SGD()
optimizer.setup(model.collect_parameters())


# How do I optimize?


# for i in xrange(0, 60000, batchsize):
#       x_batch = x_train[indexes[i : i + batchsize]]
#       y_batch = y_train[indexes[i : i + batchsize]]
#
#       optimizer.zero_grads()
#       loss, accuracy = forward(x_batch, y_batch)
#       loss.backward()
#       optimizer.update()
