import json,subprocess,re,time,urllib.parse,os
seeds=json.load(open('seeds/icse100.json'))
def norm(t): return re.sub(r'[^a-z0-9]','',t.lower())
out_f='seeds/icse100_arxiv.json'; found=json.load(open(out_f)) if os.path.exists(out_f) else {}
miss=[s for s in seeds if not s.get("pdf") and s["id"] not in found]
for s in miss:
    words=[w for w in re.sub(r'[^\w\s]',' ',s["title"]).split() if len(w)>3][:6]
    q=urllib.parse.quote(" AND ".join(f'ti:{w}' for w in words))
    x=""
    for attempt in range(4):
        x=subprocess.run(["curl","-s","-m","40",f"https://export.arxiv.org/api/query?search_query={q}&max_results=5"],capture_output=True,text=True).stdout
        if "Rate exceeded" in x or not x.strip(): time.sleep(15*(attempt+1)); continue
        break
    hit=""
    for m in re.finditer(r'<entry>(.*?)</entry>',x,re.S):
        t=re.sub(r'\s+',' ',re.search(r'<title>(.*?)</title>',m.group(1),re.S).group(1)).strip()
        if norm(t)[:28]==norm(s["title"])[:28]: hit=re.search(r'arxiv\.org/abs/([0-9.]+)',m.group(1)).group(1); break
    found[s["id"]]=hit; json.dump(found,open(out_f,'w'),indent=1)
    time.sleep(5)
print("done; hits:",sum(1 for v in found.values() if v),"of",len(found))
