all: slide.pdf  lesson01.out game-of-life.out loop.out gpu.out loop-1.out loop-2.out
slide.pdf:slide.tex
	platex --shell-escape slide.tex
#	bibtex slide
	platex --shell-escape slide.tex
	platex --shell-escape slide.tex
	dvipdfmx slide.dvi
lesson01.out: lesson01.cpp
	g++ lesson01.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o lesson01.out
#	g++ lesson01.cpp -I Halide/include/
#       to execute, create symbolic-link of the Halide folder and set
#	LD_LIBRARY_PATH=Halide/bin ./lesson_01

game-of-life.out: game-of-life.cpp
	g++ game-of-life.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o game-of-life.out


loop.out: loop.cpp
	g++ loop.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o loop.out


gpu.out: gpu.cpp
	g++ gpu.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o gpu.out


loop-1.out: loop-1.cpp
	g++ loop-1.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o loop-1.out


loop-2.out: loop-2.cpp
	g++ loop-2.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o loop-2.out
