Tmp=/tmp/$(HOME)/auk

$(Tmp)/%.awk : %.auk
	' > $@

run: 
	$(MAKE) $(Tmp)/$f.awk
	$(MAKE) $(Tmp)/*.awk
	AWKPATH="$(Tmp):$(AWKPATH)" gawk -f $f.awkf

#!/usr/bin/env bash

x=$1
shift
for f in *.auk; do
  gawk '{print gensub( /\.([^0-9\\*\\$\\+])([a-zA-Z0-9_]*)/, "[\"\\1\\2\"]","g",$0)}' $1 > $(Tmp)/${f%auk}awk
done
