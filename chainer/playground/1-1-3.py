#!/usr/bin/env python
# -*- coding: utf-8 -*-"

"""
0 .. 9までのMNISTデータを１つづつ、標準出力に可視化してみよ。
"""

from chainer import datasets

train, test = datasets.get_mnist()


for label_i_want in xrange(10):
    for labeled_image in train:
        image, label = labeled_image
        if label != label_i_want:
            continue
        print label
        for i in xrange(28):
            print "".join([str(n)[2] for n in image[28*i:28*(i+1)]]).replace("0"," ")
        break
