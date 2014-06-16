#include <Halide.h>
#include <stdio.h>

const int NX=10,NY=10;

int main(int argc, char **argv) {

  Halide::Func cell("cell");
  Halide::Var x, y;
  cell(x, y) = 0;
  cell(2, 1) = 1;
  cell(3, 2) = 1;
  cell(1, 3) = 1;
  cell(2, 3) = 1;
  cell(3, 3) = 1;

  Halide::Image<int32_t> output;
  output = cell.realize(NX,NY);

  Halide::ImageParam input(Halide::Int(32), 2); // int32_t 2D


  Halide::Func cell2;
  cell2(x,y)=input(x,y)+1;
  Halide::Func nbd("nbd");
  Halide::Func bc("bc");

  for (int t=0; t<10; ++t) {
    Halide::Image<int32_t> output = cell.realize(NX,NY);
    for (int j = 0; j < output.height(); j++) {
      for (int i = 0; i < output.width(); i++) {
	printf("%1d", output(i, j));
      }
      printf("\n");
    }
    printf("\n");

    bc(x,y)=select((x>=0)&&(x<NX)&&(y>=0)&&(y<NY)
		   ,output(min(NX-1,max(x,0)),min(NY-1,max(y,0))),0);
    
    nbd(x,y)=-bc(x,y);
    // staged programming!
    for(int dy=-1; dy<=1; ++dy) {   
      for(int dx=-1; dx<=1; ++dx) {
	nbd(x,y)+=bc(x+dx,y+dy);
      }
    }

    cell(x,y)=select((bc(x,y)==1&&nbd(x,y)==3)||(bc(x,y)==1&&nbd(x,y)==2)
		     || (bc(x,y)==0&&nbd(x,y)==3)
		     ,1,0);
  }

  return 0;
}
