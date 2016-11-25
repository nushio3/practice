# -*- coding: utf-8 -*-"

import numpy as np
import math
import scipy.ndimage as ndimage

img = ndimage.imread("eight.png",flatten=True)
img = ((255 - img)/255.0).astype(np.float32)

img = img.reshape((784,))

print img

print img.shape

def visualize_mnist_image(img):
    def float_to_char(x):
        if x <= 0:
            return ' '
        if x >= 1:
            return '█'
        i = int(math.floor(5*x))
        return [" ","░", "▒", "▓", "█"][i]

    for i in xrange(28):
        print "".join([float_to_char(n) for n in img[28*i:28*(i+1)]])


visualize_mnist_image(img)
