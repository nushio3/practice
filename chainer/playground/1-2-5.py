#!/usr/bin/env
# -*- coding: utf-8 -*-"

import numpy as np
from chainer import Variable
from chainer import functions as F

x_data = np.array([5], dtype=np.float32)
x = Variable(x_data)
y = F.relu(x)

z = Variable(np.array([[10, 20], [30, 40]], dtype=np.float32))
zz = F.transpose(z)
print(zz.data)


x_data = np.array([3,4,5], dtype=np.float32)
x = Variable(x_data)
y = F.sum(F.exp(x)+F.sin(x))
y.backward()



print(x.grad)
