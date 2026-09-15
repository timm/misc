"""depends: aggregate reader JSONs into a nine-slot matrix + CSV.
usage: python3 aggregate.py <readers_dir> <seeds.json> <out_prefix>
seeds.json: list of {slug, title, year, cites, first_author, ...}; reader JSON per slug in readers_dir."""
import json,sys,os,collections,csv,numpy as np
from scipy.cluster.hierarchy import linkage, leaves_list, to_tree
from scipy.spatial.distance import squareform
sys.path.insert(0,os.path.dirname(os.path.abspath(__file__)))
from canon import canon
rd,seedf,out=sys.argv[1:4]
seeds=json.load(open(seedf)); TYPES=["steppingstone","statmethod","methodology","dataset","software","sanitycheck","numericfact","theory","replication"]
PAPERS=[s["slug"] for s in seeds if not s.get("duplicate_of") and os.path.exists(os.path.join(rd,s["slug"]+".json"))]
LABEL={s["slug"]:f"{s.get('first_author','').split()[-1] if s.get('first_author') else ''} {s.get('year','')}: {s['title'][:38]}{'…' if len(s['title'])>38 else ''}" for s in seeds}
edges=[]
for p in PAPERS:
    d=json.load(open(os.path.join(rd,p+".json")))
    for e in d["edges"]:
        e=dict(e); e["paper"]=p; e["canon"]=canon(e["target"]); e["polarity"]=e.get("polarity") or "neutral"
        for k in ("self","executed","transitive"): e[k]=bool(e.get(k,False))
        if e["type"] not in TYPES: e["type"]="sanitycheck"
        edges.append(e)
pp=collections.defaultdict(set); cells=collections.defaultdict(list)
for e in edges: pp[e["canon"]].add(e["paper"]); cells[(e["canon"],e["paper"])].append(e)
targets=sorted(pp,key=lambda t:(-len(pp[t]),t))
def vec(t,p):
    ts={e["type"] for e in cells.get((t,p),[])}; return [1 if ty in ts else 0 for ty in TYPES]
X=np.array([[v for p in PAPERS for v in vec(t,p)] for t in targets],float); Y=np.array([[v for t in targets for v in vec(t,p)] for p in PAPERS],float)
def clus(A):
    if len(A)<2: return list(range(len(A))),{"leaf":0,"h":0.0},1.0
    Dm=squareform(np.abs(A[:,None,:]-A[None,:,:]).sum(2)); L=linkage(Dm,'average')
    def rec(n): return {"leaf":int(n.id),"h":0.0} if n.is_leaf() else {"h":float(n.dist),"l":rec(n.left),"r":rec(n.right)}
    return [int(x) for x in leaves_list(L)],rec(to_tree(L)),float(Dm.max() or 1)
ro,rt,rmax=clus(X); co,ct,cmax=clus(Y)
grid=[]
for t in targets:
    row=[]
    for p in PAPERS:
        es=cells.get((t,p),[])
        if not es: row.append(None); continue
        slots={}
        for ty in TYPES:
            sub=[e for e in es if e["type"]==ty]
            if sub: slots[ty]={"conf":collections.Counter(e["confidence"] for e in sub).most_common(1)[0][0],"self":any(e["self"] for e in sub),"executed":any(e["executed"] for e in sub),
                "polarity":"refutes" if any(e["polarity"]=="refutes" for e in sub) else ("supports" if any(e["polarity"]=="supports" for e in sub) else "neutral"),"transitive":all(e["transitive"] for e in sub)}
        row.append({"slots":slots,"ev":[{"type":e["type"],"q":e["evidence"],"raw":e["target"],"conf":e["confidence"],"sec":e.get("section",""),"self":e["self"],"executed":e["executed"],"polarity":e["polarity"],"transitive":e["transitive"]} for e in es]})
    grid.append(row)
M={"papers":PAPERS,"plabels":[LABEL[p] for p in PAPERS],"targets":targets,"indeg":[len(pp[t]) for t in targets],
   "tself":[any(e["self"] for e in edges if e["canon"]==t) for t in targets],
   "ttype":[[sum(1 for e in edges if e["canon"]==t and e["type"]==ty) for ty in TYPES] for t in targets],
   "ptype":[[sum(1 for e in edges if e["paper"]==p and e["type"]==ty) for ty in TYPES] for p in PAPERS],
   "grid":grid,"rowOrder":ro,"colOrder":co,"rowTree":rt,"colTree":ct,"rowMax":rmax,"colMax":cmax,"types":TYPES,
   "totals":{ty:sum(1 for e in edges if e["type"]==ty) for ty in TYPES},"nedges":len(edges)}
json.dump(M,open(out+"_matrix.json","w"))
with open(out+"_edges.csv","w",newline="") as fh:
    w=csv.writer(fh); w.writerow(["paper","target_canonical","target_raw","type","self","executed","polarity","transitive","confidence","section","evidence","ref","doi_or_url"])
    for e in edges: w.writerow([e["paper"],e["canon"],e["target"],e["type"],e["self"],e["executed"],e["polarity"],e["transitive"],e["confidence"],e.get("section",""),e["evidence"],e.get("ref",""),e.get("doi_or_url","")])
print(json.dumps({"papers":len(PAPERS),"edges":len(edges),"targets":len(targets),"types":M["totals"],"self":sum(e["self"] for e in edges),"executed":sum(e["executed"] for e in edges),"refutes":sum(e["polarity"]=="refutes" for e in edges),"top":[(t,len(pp[t])) for t in targets[:10]]},indent=1))
