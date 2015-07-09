from operator import mul

import numpy as np
import chainer
from chainer import computational_graph as c
from chainer import cuda, Variable, FunctionSet, optimizers
import chainer.functions as F
from chainer import optimizers



test_pat = [
    [1,1,0,0,0,0,0,0]  ,
    [4,1,1,0,0,0,0,0]  ,
    [0,4,1,1,2,2,2,2]  ,
    [0,0,4,1,1,0,0,0]  ,
    [0,4,1,1,1,1,3,3]  ,
    [4,1,1,0,0,1,1,0]  ,
    [1,1,0,0,0,0,1,1]
  ]

test_pat2= [
    [0,8,8,0,8,8,0,0]  ,
    [0,8,8,1,8,8,0,0]  ,
    [0,1,0,0,0,0,1,0]  ,
    [1,0,0,0,0,0,8,8]  ,
    [8,8,0,0,0,0,8,8]  ,
    [8,8,1,8,8,1,0,0]  ,
    [0,0,0,8,8,0,0,0]
  ]

data = np.array([[test_pat,test_pat2]])

v = Variable(data)


def zoom_x2(batch):
    shape = batch.data.shape
    channel_shape = shape[0:-2]
    height, width = shape[-2:]

    volume = reduce(mul,shape,1)

    b1 = F.reshape(batch,(volume,1))
    b2 = F.concat([b1,b1],1)

    b3 = F.reshape(b2,(volume/width,2*width))
    b4 = F.concat([b3,b3],1)

    return F.reshape(b4, channel_shape + (2*height ,) + (2*width ,))


print "original images"
print v.data
print "images x2"
print zoom_x2(v).data

# this zoom function is
print F.mean_squared_error(F.average_pooling_2d(zoom_x2(v),2) , v).data == 0
print F.mean_squared_error(F.max_pooling_2d(zoom_x2(v),2)     , v).data == 0
