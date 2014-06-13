#include <Halide.h>
#include <stdio.h>

const int NX=10,NY=10;

int main(int argc, char **argv) {

  Halide::Func cell("cell");
  Halide::Var x,y;
  cell(x, y) = 0;

  
  Halide::Image<int32_t> output;
 
  output = cell.realize(NX,NY);



  for (int t=0; t<10; ++t) {
    printf("\n");
    for (int j = 0; j < output.height(); j++) {
      for (int i = 0; i < output.width(); i++) {
	printf("%1d", output(i, j));
      }
      printf("\n");
    }

    cell(x,y)=output(x,y)+1;
    output = cell.realize(NX,NY);

  }

  return 0;
}
