saved:
	git commit -am saved; git push; git status

FILES=$(wildcard *.lhs)
docs: $(FILES:.lhs=.md)

%.md : %.lhs
	@echo "$^ ==> $@"
	@gawk -f hs2md.awk $^ > $@
