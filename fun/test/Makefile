MAKEFLAGS += --silent
SHELL:= /bin/bash#
PATH := $(HOME)/opt/fun/bin:$(PATH)#

.PHONY : help
help : Makefile
	echo ""; echo "usage: "
	@gawk 'BEGIN {FS="[ \t]*:.*##[ \t]*"}  \
	  NF==2 { printf \
           "  \033[36m%-15s\033[0m %s\n","make " $$1,$$2}'  $< \
	| grep -v gawk

ok:
	bash ../fun

#col2=gawk -F, '{print $$2}' | sort -n | uniq -c
#sort2=sort -n -t, -k 2  | gawk -F, '{print $$2}'  | cat -n
# title: Data Mining for Very Busy People
clean1 : ok ## remove strange thins
	 @cat ../data/bad.csv | clean
col1 : ok ## cull dull columns
	 @$(MAKE) clean1  | cols 
bins1 : ok ## unsupervised discretization 
	@cat data/weather.csv | clean | cols 
	#| $(sort2)
	@#cat data/weather.csv | clean | cols |  bins | $(sort2)

bins2  :ok; 
	@  cat  data/auto93.csv | $(col2)
	@  bins --data data/auto93.csv | $(col2)


