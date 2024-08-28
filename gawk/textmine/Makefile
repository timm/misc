HALL=$(HOME)/gits/zhe/src/workspace/data/Hall.csv
.SILENT:

eg1:
	gawk -f expand.awk $(HALL) | gawk -f stop.awk | gawk -f stats.awk
