fc = 'result/AdaGrad_4a890928f263a66f5ec4c6302631cfc5_current.txt'
fp = 'result/AdaGrad_4a890928f263a66f5ec4c6302631cfc5_prediction.txt'
plot fc w l t 'curve', fp  w l t 'prediction'
pause 1
reread
