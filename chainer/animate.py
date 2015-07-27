#!/usr/bin/env python

import subprocess

result_dir = 'result-2015-06-29'

candidates=[]

# for o in "cmaes AdaDelta AdaGrad Adam MomentumSGD RMSprop SGD".split():

for o in "RMSprop".split():
    ranking = []
    for i in range(6):
        i = 'DUAL_RAND'
        fn = '{}/log-{}-{}.txt'.format(result_dir, o, i)
        print fn
        p = subprocess.Popen('tail -n 1 {}'.format(fn), shell=True, stdout=subprocess.PIPE)
        stdout_str, stderr_str = p.communicate()
        time_str, score_str = stdout_str.split()
        ranking.append((float(score_str), o,i))
    ranking.sort()

    print ranking
    candidates.append(ranking[0])
    candidates.append(ranking[-1])

print candidates

for (score, o, i) in candidates:


    fn = '{}/log-{}-{}.txt'.format(result_dir, o, i)
    print fn
    with open("tmp.gnu", "w") as fp:
        fp.write("""
        set term png 20 size 1024,768
        set out '{pngfn}'
        set grid
        set title '{fn}'
        set yrange[:0]
        set xlabel 'iteration'
        set ylabel 'score'
        plot '{fn}' w l lw 2 t ''

        """.format(fn=fn, pngfn=fn.replace('.txt','.png')))
    subprocess.call('gnuplot tmp.gnu',shell=True)


    o='RMSpropAdaGrad'
    cmd = 'ls {}/{}-{}-*.txt'.format(result_dir, o, i)




    p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
    stdout_str, stderr_str = p.communicate()
    lastpng=''
    for fn in stdout_str.split():
        lastpng=fn.replace('.txt','.png')
        with open("tmp.gnu", "w") as fp:
            fp.write("""
            set term png
            set out '{pngfn}'
            set size ratio -1
            set grid
            set title '{datafn}'
            plot '{datafn}' pt 7 t ''

            """.format(datafn=fn, pngfn=lastpng))
        subprocess.call('gnuplot tmp.gnu',shell=True)

    cmd = 'convert  -delay 5  {rd}/{o}-{i}-*.png  -delay 500 {lastpng} {rd}/{o}-{i}.gif'.format(rd=result_dir,o=o,i=i,lastpng=lastpng)
    subprocess.call(cmd,shell=True)
