ok: auto93.csv
	
auto93.csv:
	ln -s ~/gits/moot/optimize/misc/auto93.csv

hc:
	python3 -B hc1.py 1 auto93.csv | sort -t, -k 3 -n | column -s, -t | cat -n

rand:
	python3 -B rand1.py 1 auto93.csv | sort -t, -k 3 -n | column -s, -t | cat -n

push:
	git commit -am saving; git push; git status
