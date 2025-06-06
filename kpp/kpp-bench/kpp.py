#!/usr/bin/env python3
import math, random

the = {
    'p': 2,
    'Few': 32,
    'Stop': 4,
    'seed': 1234567891,
    'file': 'big.csv'
}
random.seed(the['seed'])

data = []
lo, hi, xcols, names = {}, {}, {}, []

def atom(x):
    try: return int(x)
    except: pass 
    try: return float(x)
    except: pass
    x = x.strip()
    if x.lower() == "true": return True
    if x.lower() == "false": return False
    return x

def split(line) : return [atom(x) for x in line.strip().split(",")]
def sumf(lst)   : return sum(lst)
def anyof(lst)  : return random.choice(lst)
def norm(c, x)  : return (x - lo[c]) / ((hi[c] - lo[c]) + 1e-32)

def xdist(r1, r2):
    d, n = 0, 0
    for c in xcols:
        a, b = norm(c, r1[c]), norm(c, r2[c])
        d += abs(a - b) ** the['p']
        n += 1
    return (d / n) ** (1 / the['p'])

def update(j, x):
    if j in xcols:
        lo[j] = x if j not in lo else min(lo[j], x)
        hi[j] = x if j not in hi else max(hi[j], x)
    return x

def read_csv(file):
    global names, data
    with open(file) as f:
        lines = [line for line in f if line.strip()]
    names = split(lines[0])
    for j, name in enumerate(names):
        if not name.endswith("+") and not name.endswith("-"):
            xcols[j] = True
    for line in lines[1:]:
        row = [update(j, x) for j, x in enumerate(split(line))]
        data.append(row)

def kpp(k=the['Stop'], rows=None):
    rows = rows or data
    centroids = [anyof(rows)]
    some = rows[:min(the['Few'], len(rows))]
    for _ in range(1, k):
        dists = [min(xdist(x, y) ** 2 for y in centroids) for x in some]
        r = random.random() * sumf(dists)
        for j, d in enumerate(dists):
            r -= d
            if r <= 0:
                centroids.append(some.pop(j))
                break
    return centroids

read_csv(the['file'])
kpp()  # output suppressed for timing

