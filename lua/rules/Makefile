##  go ahead...    O       O
##   ... make     T/\     /\=,---.
##   my day       U /\   /\   `O'    cww


HOME       = <a href="http://github.com/timm/ezr">home</a>
CONTRIBUTE = <a href="https://github.com/timm/ezr/blob/main/CONRIBUTE.md">contribute</a>
LICENSE    = <a href="https://github.com/timm/ezr/blob/main/LICENSE.md">license</a>
ISSUES     = <a href="http://github.com/timm/ezr/issues">issues</a>

MENU       = $(HOME) | $(CONTRIBUTE) | $(ISSUES) | $(LICENSE)

IMAGE      = <img src="img/ezr.png" align=right width=150>
CSS        = p { text-align: right;} pre,code {font-size: x-small;}

~/tmp/%.html : %.lua
	pycco -d ~/tmp $<
	echo "p {text-align: right;}"       >> ~/tmp/pycco.css
	echo "code,pre {font-size: small;}" >> ~/tmp/pycco.css
	open $@
