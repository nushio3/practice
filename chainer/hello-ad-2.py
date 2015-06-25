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


def forward(x_data):
    f = model.l1
    b = f(0*x_data)
    w = f(x_data+1) - f(x_data)
    y = w**2 + 4*w + 4 + b**2 -6*b+9

    # The following leads to divergence.
    # y = -(w**2 + 4*w + 4 + b**2 -6*b+9)
    return y

while True:
    optimizer.zero_grads()
    x = Variable(np.array([0], dtype=np.float32))
    y = forward(x)
    y.backward()
    optimizer.update()
    print (model.l1.W, model.l1.b)
