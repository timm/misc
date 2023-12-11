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
  Cplx= {Acap=sw46, Pcap=sw46, Tool=sw46}, --12
  Ltex= {Pcap=nw4},  -- 4
  Pmat= {Acap=nw,   Pcap=sw46}, -- 6
  Pvol= {Plex=sw},  --2
  Rely= {Acap=sw4,  Pcap=sw4,  Pmat=sw4}, -- 12
  Ruse= {Aexp=sw46, Ltex=sw46},  --8
  Sced= {Cplx=ne46, Time=ne46, Pcap=nw4, Aexp=nw4, Acap=nw4,
         Plex=nw4,  Ltex=nw,   Pmat=nw,  Rely=ne,  Pvol=ne, Tool=nw}, -- 34
  Stor= {Acap=sw46, Pcap=sw46}, --8
  Team= {Aexp=nw,   Sced=nw,   Site=nw}, --6
  Time= {Acap=sw46, Pcap=sw46, Tool=sw26}, --10
  Tool= {Acap=nw,   Pcap=nw,   Pmat=nw} -- 6
}
