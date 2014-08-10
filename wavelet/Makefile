all: gsl-example.out 2d.out

GSLC=g++ -I/usr/include/ -L/usr/lib32 -lgsl -lm -lgslcblas

gsl-example.out: gsl-example.c
	$(GSLC) gsl-example.c -o gsl-example.out

2d.out: 2d.c
	$(GSLC) 2d.c -o 2d.out
