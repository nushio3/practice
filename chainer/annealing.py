import random,math

def temperature(ib):
    return 1e-4*10**(float(ib)*0.25)

def fluctuation(ib):
    return 1e-2*10**(float(ib)*0.25)

def perturb(xs,ib):
    f = fluctuation(ib)
    return [x+f*(2*random.random()-1) for x in xs]

def annealing(func, xs0,nbath):
    initial_members = [perturb(xs0,ib) for ib in range(nbath)]
    bath = [(func(xs),xs) for xs in initial_members]

    for t in range(6000):
        for ib in range(nbath):
            e,xs = bath[ib]

            challengers=[]
            if ib>0:
                challengers.append(bath[ib-1])
            if ib<nbath-1:
                challengers.append(bath[ib+1])
            xs_new=perturb(xs,ib)
            e_new=func(xs_new)
            challengers.append((e_new,xs_new))
            for (e2,xs2) in challengers:
                e,xs = bath[ib]
                if e2<=e or random.random() < math.exp((e-e2)/temperature(ib)):
                    bath[ib]=(e2,xs2)
            if ib==0:
                print bath[0][0]
    return sorted(bath)[0]
