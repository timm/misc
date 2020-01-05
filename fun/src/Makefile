col2=gawk -F, '{print $$2}' | sort -n | uniq -c
sort2=sort -n -t, -k 2  | gawk -F, '{print $$2}'  | cat -n
# title: Data Mining for Very Busy People
clean1 :; @cat data/bad.csv | clea
col1   :; @$(MAKE) clean1  | cols 
bins1  :; 
	@cat data/weather.csv | clean | cols | $(sort2)
	@#cat data/weather.csv | clean | cols |  bins | $(sort2)

bins2  :; 
	@  cat  data/auto93.csv | $(col2)
	@  bins --data data/auto93.csv | $(col2)


