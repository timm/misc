-- vim : ft=lua ts=2 sw=2 et:

-------
-- Return valid ranges in Cocomo

--- @export
local p,n,s="+","-","*"
return {
  acap= {n}, cplx={p,1,6}, prec={s,1,6},
	aexp= {n}, data={p,2,5}, flex={s,1,6},
	ltex= {n}, docu={p},     arch={s,1,6},
	pcap= {n}, pvol={p,2,5}, team={s,1,6},
	pcon= {n}, rely={p},     pmat={s,1,6},
	plex= {n}, ruse={p,2,6},
	sced= {n}, stor={p,3,6},
	site= {n}, time={p,3,6},
  tool= {n}
}
