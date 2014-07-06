EXE=./dist/build/solar-physics/solar-physics
PDF=./output/main.pdf

all: $(PDF)

$(PDF):$(EXE)
	$(EXE)
	(cd output; ./run.sh)

$(EXE):Main.hs material/template.tex
	cabal build