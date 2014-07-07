platex --shell-escape main.tex
bibtex main
platex --shell-escape main.tex
platex --shell-escape main.tex
dvipdfmx main.dvi
