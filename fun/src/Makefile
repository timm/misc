## Usage: 
##   make help

.PHONY: clean
clean: ../data/bad.csv
	@cat $< | gawk -f clean.awk
 
cols:
	@$(MAKE) clean | gawk -f cols.awk 

