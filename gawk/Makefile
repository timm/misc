CDATA=~/gits/timm/moot/classify/diabetes.csv

nbc:
	@chmod +x nbc.awk
	D@./bin.awk $(CDATA) | ./nbc.awk | sort -n | uniq -c

confuse:
	@chmod +x nbc.awk
	@./bin.awk $(CDATA) | ./nbc.awk | ./confuse.awk

