-- vim : ft=lua ts=2 sw=2 et:
local  _,ne,nw,nw4,sw,sw4,ne46,w26,sw46

_  = 0

ne={{_,_,_,1,2,_}, -- bad if lohi
  {_,_,_,_,1,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_}}
nw={{2,1,_,_,_,_}, -- bad if lolo
  {1,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_}}
nw4={{4,2,1,_,_,_}, -- very bad if  lolo
  {2,1,_,_,_,_},
  {1,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_}}
sw={{_,_,_,_,_,_}, -- bad if  hilo
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {1,_,_,_,_,_},
  {2,1,_,_,_,_},
  {_,_,_,_,_,_}}
sw4={{_,_,_,_,_,_}, -- very bad if  hilo
  {_,_,_,_,_,_},
  {1,_,_,_,_,_},
  {2,1,_,_,_,_},
  {4,2,1,_,_,_},
  {_,_,_,_,_,_}}
-- bounded by 1..6
ne46={{_,_,_,1,2,4}, -- very bad if lohi
  {_,_,_,_,1,2},
  {_,_,_,_,_,1},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_}}
sw26={{_,_,_,_,_,_}, -- bad if hilo
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {1,_,_,_,_,_},
  {2,1,_,_,_,_}}
sw46={{_,_,_,_,_,_}, -- very bad if hilo
  {_,_,_,_,_,_},
  {_,_,_,_,_,_},
  {1,_,_,_,_,_},
  {2,1,_,_,_,_},
  {4,2,1,_,_,_}}

return {
  cplx= {acap=sw46, pcap=sw46, tool=sw46}, --12
  ltex= {pcap=nw4},  -- 4
  pmat= {acap=nw,   pcap=sw46}, -- 6
  pvol= {plex=sw},  --2
  rely= {acap=sw4,  pcap=sw4,  pmat=sw4}, -- 12
  ruse= {aexp=sw46, ltex=sw46},  --8
  sced= {cplx=ne46, time=ne46, pcap=nw4, aexp=nw4, acap=nw4,
         plex=nw4,  ltex=nw, pmat=nw, rely=ne, pvol=ne, tool=nw}, -- 34
  stor= {acap=sw46, pcap=sw46}, --8
  team= {aexp=nw,   sced=nw,  site=nw}, --6
  time= {acap=sw46, pcap=sw46, tool=sw26}, --10
  tool= {acap=nw,   pcap=nw,  pmat=nw} -- 6
}
