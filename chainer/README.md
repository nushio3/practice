Chainer Optimizer Comparison
====

40次元の変数 $(x_1, ... , x_20, y_1, ... , y_20)$ に対して、$\\sum_{i<j} V(i,j) + \\sum_i V(i) $を最小化しています。ここで

```math
V(i,j)=-2d(i,j)^{-6}+d(i,j)^{-12}
V(i)=0.01 \cdot {x_i}^2+{y_i}^2
d(i,j)=\sqr{(x_i-x_j)^2+(y_i-y_j)^2}
```

Chainerを呼び出しているソースはこれ https://github.com/nushio3/practice/blob/chainer-test/chainer/chainer-optimizer-test.py
`potential_function`という関数が、cmaes用の普通のpythonの浮動小数点値の関数としても、chainer経由で自動微分されるVariableの関数としても、まったく同一のソースコードで使いまわせるのがすばらしい。


全手法の比較

![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/optimization-progress.png)


以下、各最適化器にたいして、最大・最小の最適化結果となったものの挙動を掲載する。

AdaDelta
---

![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-AdaDelta-0.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-AdaDelta-2.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/AdaDelta-0.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/AdaDelta-2.gif)

AdaGrad
----
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-AdaGrad-4.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-AdaGrad-5.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/AdaGrad-4.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/AdaGrad-5.gif)

Adam
---
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-Adam-1.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-Adam-3.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/Adam-1.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/Adam-3.gif)

MomentumSGD
----
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-MomentumSGD-2.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-MomentumSGD-5.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/MomentumSGD-2.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/MomentumSGD-5.gif)

RMSProp
---
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-RMSprop-3.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-RMSprop-4.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/RMSprop-3.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/RMSprop-4.gif)

SGD
---
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-SGD-3.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-SGD-5.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/SGD-3.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/SGD-5.gif)

cmaes
---
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-cmaes-2.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/log-cmaes-4.png)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/cmaes-2.gif)
![img](https://raw.githubusercontent.com/nushio3/practice/chainer-test/chainer/gif/cmaes-4.gif)
