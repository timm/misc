#.SILENT:
all: ready $(wildcard docs/*.html)

ready:
	mkdir -p etc tmp test docs

FMT= -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition

define html
echo "# $@ ..."
python3 -m markdown $(FMT) $^ > tmp_md
./etc/page.sh tmp_md  > $@
endef

define roadmap
[asdas](as) | [assss](33) | [xkss](3223)

endef
export roadmap

define headera
<!DOCTYPE html>
<html>
<head>

endef
export headera

define headerz
<meta charset="UTF-8">
<link href="https://fonts.googleapis.com/css?family=Source+Code+Pro|Source+Sans+Pro:300,400,400i,700" rel="stylesheet">
<style>
body  { 
  padding: 10px; 
	font-family: Source Sans Pro, Roboto, Open Sans, Liberation Sans, DejaVu Sans, Verdana, Helvetica, Arial, sans-serif;
}
a                 { color: #CC0000; text-decoration: none; font-weight: bold; }
a:hover           { text-decoration: underline; }
h1,h2,h3,h4       { color: #CC0000; } 
.admonition       { border: 1px solid #EEE; box-shadow: 2px 2px 3px black; }
.admonition-title { font-weight: bold; }
pre   { 
	font-weight: normal;
	padding: 5px;
	border-left: 1px dotted black;
	background-image: linear-gradient(to right, #EEE, white);
	margin-left: 30px;
	padding: 5px;
}
</style>
</head>
<body>


endef
export headerz

# to build this : if any of this is new      ; then do this
docs/style.css  : etc/style.css              ; cp $^ $@
docs/%.html     : tmp/%.md docs/style.css    ; $(call html)
docs/%.html     : %.md     docs/style.css    ; $(call html)

tmp/%.md : %.lua
	(echo $$roadmap; echo "\n\n# $(FILENAME)"; gawk -f etc/lua2md.awk $^) > $@

tmp/%.html : tmp/%.md
	echo $$headera > $@
	echo "<title>$(basename $(notdir $@))</title>" >> $@
	echo $$headerz >> $@
	echo "<img align=right width=250 src='dots4.png'>" >> $@
	echo "<h1>$(basename $(notdir $@))</h1>" >> $@
	echo "<p><em>Tim Menzies, 2024<br>timm@ieee.org</em></p>" >> $@
	python3 -m markdown $(FMT) $^ >> $@
	echo "</body></html>" >> $@

tmp/%.md : %.lisp
	gawk -f etc/list2md.awk $^ > $@

tmp/%.md : %.py
	echo "# $@ ..."
	gawk -f etc/py2md.awk $^ > $@

put:
	git commit -am saving; git push; git status
