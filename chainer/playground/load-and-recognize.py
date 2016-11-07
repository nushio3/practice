# -*- coding: utf-8 -*-"

import chainer
from chainer import datasets
from chainer import serializers
from chainer import links as L
from chainer import functions as F
from chainer import Variable, optimizers
import numpy as np
import scipy.ndimage as ndimage


class MLP(chainer.Chain):
    def __init__(self, n_units, n_out):
        super(MLP, self).__init__(
            # the size of the inputs to each layer will be inferred
            l1=L.Linear(None, n_units),  # n_in -> n_units
            l2=L.Linear(None, n_units),  # n_units -> n_units
            l3=L.Linear(None, n_out),  # n_units -> n_out
        )

    def __call__(self, x):
        h1 = F.relu(self.l1(x))
        h2 = F.relu(self.l2(h1))
        return self.l3(h2)

def MLP_response_to_label(y):
    return np.argmax(y[0])


batchsize = 100
train, test = chainer.datasets.get_mnist()
train_iter = chainer.iterators.SerialIterator(train, batchsize)
test_iter = chainer.iterators.SerialIterator(test, batchsize, repeat=False, shuffle=False)

model = MLP(784, 10)

# load the model from the saved learning
serializers.load_npz('mnist.model', model)


train, test = datasets.get_mnist()
for t in test[0:10]:
    img_data, label = t
    x = Variable(img_data.reshape((1,784)))

    the_y = model(x)
    print label, MLP_response_to_label(the_y.data)


img = ndimage.imread("five.png",flatten=True)
img = ((255 - img)/255.0).astype(np.float32)
the_img = img.reshape((1,784))

x = Variable(the_img)

the_y = model(x)
print "The given image is ", MLP_response_to_label(the_y.data)
