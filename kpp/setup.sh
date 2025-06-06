#!/usr/bin/env bash
# setup-kpp.sh : generate big.csv and 3 kpp scripts + runner

mkdir -p kpp-bench
cd kpp-bench

# --------------------------------
# 1. Generate CSV with Python
# --------------------------------
cat > make-big.py <<'EOF'
import random, csv
with open("big.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow([f"x{i}" for i in range(20)] + ["y+"])
    for _ in range(100_000):
        row = [round(random.uniform(0, 1), 4) for _ in range(20)]
        row.append(round(random.uniform(0, 1), 4))
        writer.writerow(row)
EOF
python3 make-big.py

# --------------------------------
# 2. kpp.js (JavaScript)
# --------------------------------
cat > kpp.js <<'EOF'
#!/usr/bin/env node
const fs = require("fs")
Math.random = require("seedrandom")(1234567891)
const the = { p: 2, Few: 32, Stop: 4, file: "big.csv" }
let data=[], lo={}, hi={}, xcols={}, names=[]
const atom=x=>isNaN(x=+x)?(x=="true"?true:x=="false"?false:x):x
const split=s=>s.split(",").map(atom)
const sum=t=>t.reduce((a,b)=>a+b,0)
const any=t=>t[Math.floor(Math.random()*t.length)]
const norm=(c,x)=>(x-lo[c])/((hi[c]-lo[c])+1e-32)
const xdist=(r1,r2)=>{let d=0,n=0;for(let c in xcols){
  let a=norm(c,r1[c]),b=norm(c,r2[c]);d+=Math.abs(a-b)**the.p;n++}
  return(d/n)**(1/the.p)}
const update=(j,x)=>{if(xcols[j]){
  lo[j]=lo[j]===undefined?x:Math.min(lo[j],x)
  hi[j]=hi[j]===undefined?x:Math.max(hi[j],x)}return x}
const readHeader=hdr=>hdr.map((name,j)=>{if(!/[+-]$/.test(name))xcols[j]=true;return name})
const readCSV=f=>{const l=fs.readFileSync(f,"utf-8").split(/\r?\n/).filter(Boolean)
  names=readHeader(l[0].split(","));data=l.slice(1).map(r=>split(r).map(update))}
const kpp=(k=the.Stop,r=data)=>{let c=[any(r)],s=r.slice(0,the.Few)
  for(let i=1;i<k;i++){let d=s.map(x=>Math.min(...c.map(y=>xdist(x,y)**2)))
  let r=Math.random()*sum(d)
  for(let j=0;j<d.length;j++)if((r-=d[j])<=0){c.push(s.splice(j,1)[0]);break}}return c}
readCSV(the.file)
kpp().forEach(r=>{}) // output suppressed for timing
EOF
chmod +x kpp.js

# --------------------------------
# 3. kpp.lua
# --------------------------------
cat > kpp.lua <<'EOF'
local the={p=2, Few=32, Stop=4, seed=1234567891, file="big.csv"}
math.randomseed(the.seed)
local push, map, sum, any = table.insert,
  function(t,f,u) u={};for i,v in ipairs(t) do u[i]=f(v,i) end;return u end,
  function(t,f) local n=0;for _,v in ipairs(t) do n=n+f(v) end;return n end,
  function(t) return t[math.random(#t)] end
local atom = function(x) return tonumber(x) or (x=="true" and true) or (x=="false" and false) or x end
local split = function(s) local t={};for x in s:gmatch("([^,]+)") do push(t,atom(x)) end;return t end
local lines = function(file) local out={} for line in io.lines(file) do push(out,line) end return out end
local Data={new=function() return setmetatable({rows={},names={},lo={},hi={},xcols={}}, {__index=Data}) end}
function Data:add(t)
  if #self.names==0 then
    self.names=t; for i,name in ipairs(t) do if not name:match("[+-]$") then self.xcols[i]=true end end
  else push(self.rows,t)
    for i,x in ipairs(t) do if self.xcols[i] then
      self.lo[i]=self.lo[i] and math.min(self.lo[i],x) or x
      self.hi[i]=self.hi[i] and math.max(self.hi[i],x) or x end end end return self end
function Data:norm(c,x) return (x - self.lo[c]) / ((self.hi[c] - self.lo[c]) + 1e-32) end
function Data:xdist(t1,t2) local d,n=0,0
  for c in pairs(self.xcols) do n=n+1; d=d+(math.abs(self:norm(c,t1[c])-self:norm(c,t2[c]))^the.p) end
  return (d/n)^(1/the.p) end
function kpp(data,k,rows)
  k=k or the.Stop; rows=rows or data.rows; local c={any(rows)}; local s={}
  for i=1,math.min(the.Few,#rows) do s[i]=rows[i] end
  for i=2,k do
    local d=map(s,function(x) return math.min(table.unpack(map(c,function(y) return data:xdist(x,y)^2 end))) end)
    local r=math.random()*sum(d,function(z) return z end)
    for j=1,#d do r=r-d[j]; if r<=0 then push(c,table.remove(s,j)); break end end end return c end
local d=Data:new(); for _,line in ipairs(lines(the.file)) do d:add(split(line)) end
kpp(d)
EOF

# --------------------------------
# 4. kpp.awk
# --------------------------------
cat > kpp.awk <<'EOF'
#!/usr/bin/env gawk -f
BEGIN{FS=OFS=",";srand(1234567891);p=2;Few=32;Stop=4}
NR==1{ncols=NF;for(i=1;i<=NF;i++)name[i]=$i;x=0;for(i=1;i<=NF;i++)xcol[i]=($i!~"[+-]$");next}
{x++;for(i=1;i<=NF;i++){a[x,i]=$i+0;if(xcol[i]){lo[i]=(x==1)?a[x,i]:(a[x,i]<lo[i]?a[x,i]:lo[i])
hi[i]=(x==1)?a[x,i]:(a[x,i]>hi[i]?a[x,i]:hi[i])}}}
function norm(i,x){return(x-lo[i])/((hi[i]-lo[i])+1e-32)}
function xdist(i,j, d,n,k){for(k=1;k<=ncols;k++)if(xcol[k]){d+=((norm(k,a[i,k])-norm(k,a[j,k]))^p);n++}
return(d/n)^(1/p)}
function kpp(k,    i,s,c,d,r,j,min,r1,r2){c[1]=int(1+rand()*x);slen=(Few<x)?Few:x
for(i=1;i<=slen;i++)s[i]=int(1+rand()*x)
for(i=2;i<=k;i++){for(j=1;j<=slen;j++){min=""
for(z=1;z<i;z++){r1=s[j];r2=c[z];d=xdist(r1,r2)^2;if(min==""||d<min)min=d}dist[j]=min}
r=rand()*sum(dist,slen);for(j=1;j<=slen;j++){r-=dist[j];if(r<=0){c[i]=s[j];break}}}}
function sum(v,n, i,s){for(i=1;i<=n;i++)s+=v[i];return s}
END{kpp(Stop)}
EOF
chmod +x kpp.awk

# --------------------------------
# 5. run-all.sh
# --------------------------------
cat > run-all.sh <<'EOF'
#!/usr/bin/env bash
echo "Running JavaScript..."
/usr/bin/time -f "\nJS Time: %U sec" ./kpp.js

echo "Running Lua..."
/usr/bin/time -f "\nLua Time: %U sec" lua kpp.lua

echo "Running Gawk..."
/usr/bin/time -f "\nGawk Time: %U sec" ./kpp.awk big.csv
EOF
chmod +x run-all.sh

echo "✅ Setup complete. Run './run-all.sh' inside ./kpp-bench"

