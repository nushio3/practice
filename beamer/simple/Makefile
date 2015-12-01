all:
	mkdir -p output
	lualatex --output-directory=output Simplest-Beamer-Demo.tex
	bibtex output/Simplest-Beamer-Demo
	lualatex --output-directory=output Simplest-Beamer-Demo.tex
	lualatex --output-directory=output Simplest-Beamer-Demo.tex
	cp output/Simplest-Beamer-Demo.pdf  Simplest-Beamer-Demo.pdf
