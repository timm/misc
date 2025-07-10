DATA := ../../../moot/optimize
TMP  := /tmp/rq1
CSV  := $(wildcard $(DATA)/binar*/*.csv)

BASENAMES := $(basename $(notdir $(CSV)))
LOGS := $(addprefix $(TMP)/,$(addsuffix .log,$(BASENAMES)))

rq1: $(LOGS)

$(TMP)/%.log: $(DATA)/%/%.csv
	@mkdir -p $(TMP)
	@echo "$@ ← $<"
	@python3 -B ezr.py -f $< --rq1 | tee $@


rq1report:
	@out="$$HOME/tmp/$(notdir $(MAKEFILE_LIST))"; \
	@tmp=/tmp/rq1; \
	@cat $$tmp/*.csv > $$out; \
	@( \
	  cat $$out | gawk -F, '{ print $$(NF), $$0 }' | \
	  sort -n -k1,1 | cut -d' ' -f2- ; \
	  cat $$out | gawk -F, -f ,/rq1.awk \
	) | column -s, -t | grep --color A
