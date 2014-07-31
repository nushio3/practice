all:   lesson01.out lesson05.out lesson08.out game-of-life.out loop.out gpu.out loop-1.out loop-2.out double-buffer.out bench-blur.out flops.out  measure-fma.out ninja-blur.out  stencil-fusion.out blur-step-fusion.out bsf-2.out



slide.pdf:slide.tex
	platex --shell-escape slide.tex
	bibtex slide
	platex --shell-escape slide.tex
	platex --shell-escape slide.tex
	dvipdfmx slide.dvi
lesson01.out: lesson01.cpp
	g++ lesson01.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o lesson01.out
#	g++ lesson01.cpp -I Halide/include/
#       to execute, create symbolic-link of the Halide folder and set
#	LD_LIBRARY_PATH=Halide/bin ./lesson_01

lesson05.out: lesson05.cpp
	g++ lesson05.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o lesson05.out

lesson08.out: lesson08.cpp
	g++ lesson08.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o lesson08.out


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

double-buffer.out: double-buffer.cpp
	g++ double-buffer.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -o double-buffer.out


blur.out: blur.cpp
	g++ blur.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o blur.out

bench-blur.out: bench-blur.cpp
	g++ -O1 bench-blur.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o bench-blur.out


measure-fma.out: measure-fma.cpp
	 clang++ measure-fma.cpp -O3 -ffp-contract=fast -march=bdver2 -S
	 clang++ measure-fma.cpp -O3 -ffp-contract=fast -march=bdver2 -o measure-fma.out

flops.out: flops.cpp
	g++ flops.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o flops.out




ninja-blur.out: ninja-blur.cpp
	g++ ninja-blur.cpp -O2 -msse2 -mfma4 -mavx -lm -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o ninja-blur.out


stencil-fusion.out: stencil-fusion.cpp
	g++ stencil-fusion.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o stencil-fusion.out


blur-step-fusion.out: blur-step-fusion.cpp
	g++ blur-step-fusion.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o blur-step-fusion.out

bsf-2.out: bsf-2.cpp
	g++ bsf-2.cpp -g -I Halide/include/ -L Halide/bin/ -lHalide -lpthread -ldl -o bsf-2.out
