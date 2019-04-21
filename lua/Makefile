F=slope.py

tidy:
	 autopep8 --in-place --aggressive --aggressive $F

publish:
	pycco -d ../docs $F
	git add ../docs/*
	git commit -am new
	git push
