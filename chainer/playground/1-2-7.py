#!/usr/bin/env python

from chainer import Chain, ChainList, Variable
from chainer import links as L
import numpy as np

x = Variable(np.array([[1, 2, 3, 4],
                       [4, 5, 6, 7],
                       [4, 5, 6, 7],
                       [7, 8, 9,10]],
                      dtype=np.float32))

x3 = Variable(np.array([[1, 2,  4],
                       [4, 5, 6],
                       [4, 5, 6],
                       [7, 8, 9]],
                      dtype=np.float32))



class MyChain(Chain):
    def __init__(self):
        super(MyChain, self).__init__(
            l1=L.Linear(4, 1),
            l2=L.Linear(1, 2),
        )

    def __call__(self, x):
        h = self.l1(x)
        return self.l2(h)


class MyChainList(ChainList):
    def __init__(self):
        super(MyChain, self).__init__(
            L.Linear(4, 3),
            L.Linear(3, 2),
        )

    def __call__(self, x):
        h = self[0](x)
        return self[1](h)


class NChainList(ChainList):
    def __init__(self,n):
        ls = []
        for i in xrange(n):
            ls.append(L.Linear(3,3))


        super(NChainList, self).__init__(
            *ls
        )

    def __call__(self, x):
        h = x
        for i in xrange(len(self)):
            h = self[i](h)
        return h




c = MyChain()
c2 = MyChainList

cn = NChainList(100)
print cn(x3).data
