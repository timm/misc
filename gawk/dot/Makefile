SHELL := /bin/bash
export PATH := $(CURDIR):$(PATH)

DATA  ?= $(HOME)/gits/moot/classify/soybean.csv

GAWK_BASE = gawk \
  -f <(dot dot.awk) \
  -f <(dot numsym.awk) \
  -f <(dot data.awk)

GAWK    = $(GAWK_BASE) -f <(dot tree.awk)
GAWK_NB = $(GAWK_BASE) -f <(dot bayes.awk)

CFG    = <(printf 'wait,400\nleaf,4\nmaxd,8\n')
CFG20  = <(printf 'wait,500\nleaf,4\nmaxd,8\n')
CFG_NB = <(printf 'wait,500\nm,1\nk,1\n')

# shell helper: shuffle DATA with given seed (default $RANDOM)
SHUF = shuf(){ gawk -v seed=$${1:-$$RANDOM} -f shuf.awk "$$2"; }

demo:
	$(GAWK) $(CFG) $(DATA) | gawk -f metrics.awk

demo-raw:
	$(GAWK) $(CFG) $(DATA)

demo20:
	@$(SHUF); for i in $$(seq 1 20); do \
	  $(GAWK) $(CFG20) <(shuf $$i $(DATA)); \
	done | gawk -f metrics.awk

nb20:
	@$(SHUF); for i in $$(seq 1 20); do \
	  $(GAWK_NB) $(CFG_NB) <(shuf $$i $(DATA)); \
	done | gawk -f metrics.awk

show:
	@for f in dot.awk numsym.awk data.awk tree.awk bayes.awk; do \
	  echo "# === $$f ==="; dot $$f; done

.PHONY: demo demo-raw demo20 nb20 show
