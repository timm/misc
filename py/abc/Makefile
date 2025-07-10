#-include ../Makefile

# DATA=../../../moot/optimize
# out="$(HOME)/tmp/$(basename "$0")"
# tmp=/tmp/rq1
# Ins=../../../moot/optimize/[mpchb]*/*.csv
# 
# 
# rq1: ../../../moot/optimize/[mpchb]*/*.csv
# 	{$(foreach f, $^, echo $f; (python3 -B ezr.py  -f $f --rq1 &);)}) | tee ~/tmp/al$@
# 
# DATA := ../../../moot/optimize
# TMP  := /tmp/rq1
# CSV  := $(wildcard $(DATA)/[m]*/*.csv)
# #CSV  := $(wildcard $(DATA)/[mpchb]*/*.csv)
# 
# # Extract just the base filenames (no dirs, no .csv)
# BASENAMES := $(basename $(notdir $(CSV)))
# LOGS := $(addprefix $(TMP)/,$(addsuffix .log,$(BASENAMES)))
# 
# all: $(LOGS)
# 
# 
# # Find the full source file for a given log file
# # Extract base name from log filename, then find full CSV
# $(TMP)/%.log:
# 	mkdir -fp $(TMP)
# 	@src="$(filter %/$*.csv,$(CSV))"; \
# 	echo "Processing $$src → $@"; \
# 	python3 -B ezr.py  -f $src --rq1 | tee $@
# 
# 
# plotcsv = python3 -c 'import sys, pandas as pd; import matplotlib.pyplot as plt; df = pd.read_csv(sys.stdin); df.plot(); plt.xlabel("$(1)"); plt.ylabel("$(2)"); plt.tight_layout(); plt.show()'
# 
# rq1Report:
# 	cat ~/tmp/alrq1 | gawk -F, 'NR>1{w+=$$9; t+=$$10} END{print w/NR, t/NR}'
# 	cat ~/tmp/alrq1 | sort -n -t, -k6,6 -k7,7 | \
# 	gawk -F, 'BEGIN {print "before (0%),check (60%),build (79%),min (100%)"} \
# 	          {OFS=","; print $$5,$$8,$$7,$$6}' | \
# 	$(call plotcsv,data sets (sorted by min),distance to heaven)

DATA := ../../../moot/optimize
TMP  := /tmp/rq1
CSV  := $(wildcard $(DATA)/[m]*/*.csv)

BASENAMES := $(basename $(notdir $(CSV)))
LOGS := $(addprefix $(TMP)/,$(addsuffix .log,$(BASENAMES)))

all: $(LOGS)

# Pattern rule with explicit dependency on source .csv
$(TMP)/%.log: $(filter %/%.csv,$(CSV))
	@mkdir -p $(TMP)
	@echo "$@ ← $^"
	@python3 -B ezr.py -f $^ --rq1 | tee $@

