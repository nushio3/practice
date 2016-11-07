#!/usr/bin/env python
# -*- coding: utf-8 -*-"

"""
0 .. 9までのMNISTデータを平均化して、、標準出力に可視化してみよ。
"""

from chainer import datasets
import numpy as np

def visualize_mnist_image(img):
    def float_to_char(x):
        if x <= 0:
            return ' '
        if x >= 1:
            return '█'
        i = int(5*x)
        return u" ░▒▓█"[i]

    for i in xrange(28):
        print "".join([float_to_char(n) for n in img[28*i:28*(i+1)]])


train, test = datasets.get_mnist()
images_of_label = {}

for i in xrange(10):
    images_of_label[i] = []


for image, label in train:
    images_of_label[label].append(image)


for i in xrange(10):
    print i, len(images_of_label[i])
    averaged_image = np.average(images_of_label[i], axis=0)
    visualize_mnist_image(averaged_image)
