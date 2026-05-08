#!/usr/bin/env python3 -B
"""
trees.py: regression / decision trees over a Data.

Local options:
    --learn.leaf=3   min examples per leaf
"""
from core import *
loadDoc(__doc__)

class Tree:
  def __init__(i, data, rows):
    i.d = clone(data, rows)
    i.ynum = adds((disty(data, r) for r in rows), Num())
    i.col, i.cut = None, 0
    i.left = i.right = None

def treeCuts(col, rows):
  vs = [r[col.at] for r in rows if r[col.at] != "?"]
  if not vs: return []
  return set(vs) if Sym == type(col) else [sorted(vs)[len(vs)//2]]

def treeSplit(data, col, cut, rows):
  l_rows, r_rows, l_num, r_num = [], [], Num(), Num()
  for row in rows:
    v  = row[col.at]
    go = v == "?" or (v == cut if Sym == type(col) else v <= cut)
    (l_rows if go else r_rows).append(row)
    add(l_num if go else r_num, disty(data, row))
  s = l_num.n*spread(l_num) + r_num.n*spread(r_num)
  return s, col, cut, l_rows, r_rows

def treeGrow(data, rows):
  tree = Tree(data, rows)
  if len(rows) >= 2 * the.learn.leaf:
    splits = (treeSplit(data, c, cut, rows)
              for c in tree.d.cols.xs
              for cut in treeCuts(c, rows))
    if valid := [s for s in splits
                 if min(len(s[3]), len(s[4])) >= the.learn.leaf]:
      _, tree.col, tree.cut, left, right = min(valid, key=lambda x: x[0])
      tree.left  = treeGrow(data, left)
      tree.right = treeGrow(data, right)
  return tree

def treeLeaf(tree, row):
  if not tree.left: return tree
  v  = row[tree.col.at]
  go = v != "?" and (v <= tree.cut if Num == type(tree.col)
                                   else v == tree.cut)
  return treeLeaf(tree.left if go else tree.right, row)

def treeNodes(tree, lvl=0, col=None, op="", cut=None):
  yield tree, lvl, col, op, cut
  if tree.col:
    ops = ("<=", ">") if Num == type(tree.col) else ("==", "!=")
    kids = sorted([(tree.left, ops[0]), (tree.right, ops[1])],
                  key=lambda z: mid(z[0].ynum))
    for k, txt in kids:
      if k: yield from treeNodes(k, lvl+1, tree.col, txt, tree.cut)

def treeShow(tree):
  for t1, lvl, col, op, cut in treeNodes(tree):
    p = f"{col.txt} {op} {o(cut)}" if col else ""
    if lvl > 0: p = "|   " * (lvl - 1) + p
    g = {c.txt: mid(c) for c in t1.d.cols.ys}
    print(f"{p:<{the.show.show}}"
          f",{o(mid(t1.ynum)):>4}"
          f" ,({t1.ynum.n:3}), {o(g)}")

__all__ = ["Tree", "treeCuts", "treeSplit", "treeGrow",
           "treeLeaf", "treeNodes", "treeShow"]
