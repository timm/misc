-include ../Makefile

rq1: ../../../moot/optimize/[mpchb]*/*.csv
	{$(foreach f, $^, (python3 -B abc.py -B 50 -f $f --acquires &);)} | tee ~/tmp/al$@


plotcsv = python3 -c 'import sys, pandas as pd; import matplotlib.pyplot as plt; df = pd.read_csv(sys.stdin); df.plot(); plt.xlabel("$(1)"); plt.ylabel("$(2)"); plt.tight_layout(); plt.show()'

rq1Report:
	cat ~/tmp/alrq1 | gawk -F, 'NR>1{w+=$$9; t+=$$10} END{print w/NR, t/NR}'
	cat ~/tmp/alrq1 | sort -n -t, -k6,6 -k7,7 | \
	gawk -F, 'BEGIN {print "before (0%),check (60%),build (79%),min (100%)"} \
	          {OFS=","; print $$5,$$8,$$7,$$6}' | \
	$(call plotcsv,data sets (sorted by min),distance to heaven)



