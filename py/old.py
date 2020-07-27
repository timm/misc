def mu(i, lo=0, hi=None):
  hi = hi or len(i.has)
  return sum([i.x(i.has[j]) for j in range(lo, hi)]) / (hi - lo)


class Some(o):
  "Collect some examples, not all."
  hi = 256  # max number of items to collect

  def __init__(i, pos=0, txt=" ", inits=[]):
    i.pos, i.txt, i.n, i._all, i.sorted = pos, txt, 0, [], False
    i.w = -1 if txt[0] == Tab.less else 1
    i.nump = txt[0] in Tab.nums
    [i.add(x) for x in inits]

  def add(i, x):
    if x != Tab.skip:
      if len(i._all) < Some.hi:
        i.update(i._all.append(x))
      elif random() < Some.hi / i.n:
        i.update(i.replace(x))
    return x

  def update(i, ignore):
    i.sorted = False
    i.n += 1

  def replace(i, x):
    i._all[int(len(i._all) * random())] = x

  def all(i):
    if not i.sorted:
      i._all = sorted(i._all)
      i.sorted = True
    return i._all

  def per(i, p=None, lo=None, hi=None): return per(i.all(), p, lo, hi)

  def sd(i, lo=None, hi=None): return sd(i.all(), lo, hi)

  def nmusd(i):
    return len(i._all), i.per(), i.sd()

  def same(i, j):
    xn, xmu, xsd = i.nmusd()
    yn, ymu, ysd = j.nmusd()
    return Ttest(xn, xmu, xsd, yn, ymu, ysd)


class Ttest(o):
  small = 0.38  # medium = 1
  d = {}
  d[95] = [[3, 3.182], [6, 2.447], [12, 2.179],
           [24, 2.064], [48, 2.011], [96, 1.985]]
  d[99] = [[3, 5.841], [6, 3.707], [12, 3.055],
           [24, 2.797], [48, 2.682], [96, 2.625]]

  def __init__(i, *lst, conf=95):
    xy = Ttest.d[conf]
    i.result = i.hedges(Ttest.small, *lst) and i.ttest(xy, *lst)

  def hedges(i, threshold, xn, xmu, xsd, yn, ymu, ysd):
    # from https://goo.gl/w62iIL
    nom = (xn - 1) * xsd ** 2 + (yn - 1) * ysd ** 2
    denom = (xn - 1) + (yn - 1)
    sp = (nom / denom)**0.5
    g = abs(xmu - ymu) / sp
    c = 1 - 3.0 / (4 * (xn + yn - 2) - 1)
    return g * c > threshold

  def ttest(i, xy, xn, xmu, xsd, yn, ymu, ysd, conf=95):
    # debugged using https://goo.gl/CRl1Bz
    t = (xmu - ymu) / max(10 ** -64, xsd**2 / xn + ysd**2 / yn)**0.5
    a = xsd ** 2 / xn
    b = ysd ** 2 / yn
    df = (a + b)**2 / (10 ** -64 + a**2 / (xn - 1) + b**2 / (yn - 1))
    c = i.critical(xy, int(df + 0.5))
    return abs(t) > c

  def critical(i, xy, df):
    x1, y1 = xy[0]
    if df < x1:
      return y1
    for x2, y2 in xy[1:]:
      if x1 <= df < x2:
        return y1 + (y2 - y1) * (df - x1) / (x2 - x1)
      x1, y1 = x2, y2
    return y2
