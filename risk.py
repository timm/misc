_  = 0

ne=[
  [_,_,_,1,2,_], # bad if lohi
  [_,_,_,_,1,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_]]
nw=[
  [2,1,_,_,_,_], # bad if lolo
  [1,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_]]
nw4=[
  [4,2,1,_,_,_], # very bad if  lolo
  [2,1,_,_,_,_],
  [1,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_]]
sw=[
  [_,_,_,_,_,_], # bad if  hilo
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [1,_,_,_,_,_],
  [2,1,_,_,_,_],
  [_,_,_,_,_,_]]
sw4=[
  [_,_,_,_,_,_], # very bad if  hilo
  [_,_,_,_,_,_],
  [1,_,_,_,_,_],
  [2,1,_,_,_,_],
  [4,2,1,_,_,_],
  [_,_,_,_,_,_]]
# bounded by 1..6
ne46=[
  [_,_,_,1,2,4], # very bad if lohi
  [_,_,_,_,1,2],
  [_,_,_,_,_,1],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_]]
sw26=[
  [_,_,_,_,_,_], # bad if hilo
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [1,_,_,_,_,_],
  [2,1,_,_,_,_]]
sw46=[
  [_,_,_,_,_,_], # very bad if hilo
  [_,_,_,_,_,_],
  [_,_,_,_,_,_],
  [1,_,_,_,_,_],
  [2,1,_,_,_,_],
  [4,2,1,_,_,_]]

return  o(
  cplx= o(acap=sw46, pcap=sw46, tool=sw46), #12
  ltex= o(pcap=nw4),  # 4
  Pmat= o(acap=nw,   pcap=sw46), # 6
  pvol= o(plex=sw),  #2
  rely= o(acap=sw4,  pcap=sw4,  Pmat=sw4), # 12
  ruse= o(aexp=sw46, ltex=sw46),  #8
  sced= o(cplx=ne46, time=ne46, pcap=nw4, aexp=nw4, acap=nw4,
         plex=nw4,  ltex=nw,   Pmat=nw,  rely=ne,  pvol=ne, tool=nw), # 34
  stor= o(acap=sw46, pcap=sw46), #8
  Team= o(aexp=nw,   sced=nw,   site=nw), #6
  time= o(acap=sw46, pcap=sw46, tool=sw26), #10
  tool= o(acap=nw,   pcap=nw,   Pmat=nw) # 6
)
