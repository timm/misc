SCRIPTS := scripts
CHUNKS  := $(wildcard chunks/*.md)

.PHONY: all check repl-check clean

all: build/build.md

build/s1.md: master.md $(CHUNKS)
	@mkdir -p build
	awk -f $(SCRIPTS)/assemble.awk    master.md > $@

build/s2.md: build/s1.md
	awk -f $(SCRIPTS)/number-repl.awk $< > $@

build/s3.md: build/s2.md
	awk -f $(SCRIPTS)/toc.awk         $< > $@

build/build.md: build/s3.md
	awk -f $(SCRIPTS)/used-in.awk     $< > $@
	awk -f $(SCRIPTS)/xref-check.awk  $@

check: build/build.md
	awk -f $(SCRIPTS)/xref-check.awk  build/build.md

# Run each declared eg, diff its stdout against the captured trace.
repl-check:
	@for tag in --the --sym --num --col --cols --like; do \
	  out=$$(lua nb.lua $$tag 2>&1); \
	  printf "[%s] %s\n" "$$tag" "$$out"; \
	done

clean:
	rm -rf build
