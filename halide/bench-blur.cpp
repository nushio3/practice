#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <stdlib.h>

 
void bench(int NX, int NY, int MAX_T) {



  Halide::Var x,y;
  Halide::Func initial_condition("initial_condition");
  initial_condition(x, y) = 0.0f;
  initial_condition(NX/3, NY/4) = 1.0f* NX*NY ;
 
  Halide::ImageParam inPar(Halide::Float(32), 2, "inPar"); // 32bit float 2D 

  Halide::Image<float_t> input = initial_condition.realize(NX,NY); 
  Halide::Image<float_t> output(NX,NY); 
  

  // precompile the programs
  Halide::Func cell2, cell3; const float a = 0.5f, b = 0.25f;
  cell2(x,y)= (a * inPar(x,y) + b * inPar((x+1)%NX,y) + b * inPar((x+NX-1)%NX,y)) ;
  cell3(x,y)= (a * cell2(x,y) + b * cell2(x,(y+1)%NY) + b * cell2(x,(y+NY-1)%NY)) ;
  
  for (int t=0; t<=MAX_T; ++t) {
    // updating logic
    inPar.set(input);
    output=cell3.realize(NX,NY);

    // swap the double-buffer
    std::swap(input, output);
  }

  // output the final state
  {
    std::ofstream ofs("debug.txt");
    float ret = 0;
    for (int j = 0; j < NY; j++) {
      for (int i = 0; i < NX; i++) {
        ret += output(i, j);
      }
    }
    ofs << ret << std::endl;
    ofs.close();
  } 
}



int main(int argc, char **argv) {
  bench(1200,800,400);
}
