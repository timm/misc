-- vim : ft=lua ts=2 sw=2 et:

-------
-- Return valid ranges in Cocomo

--- @export
local p,n,s="+","-","*"
return {
  Acap= {n}, Cplx={p,1,6}, Prec={s,1,6},
	Aexp= {n}, Data={p,2,5}, Flex={s,1,6},
	Ltex= {n}, Docu={p},     Arch={s,1,6},
	Pcap= {n}, Pvol={p,2,5}, Team={s,1,6},
	Pcon= {n}, Rely={p},     Pmat={s,1,6},
	Plex= {n}, Ruse={p,2,6},
	Sced= {n}, Stor={p,3,6},
	Site= {n}, Time={p,3,6},
  Tool= {n}
}
