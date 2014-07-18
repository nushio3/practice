#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <stdlib.h>
#include <sys/time.h>

using namespace std;

namespace{
  double wtime() {
    timeval tv;
    gettimeofday(&tv,NULL);
    return double(tv.tv_sec) + double(tv.tv_usec)*1e-6;
  }
}

const int NX = 64,NY=64;

int main (){
  
  Halide::Var x,y;
  Halide::Func initial_condition("ic");
  initial_condition(x,y) = 1.0f / (1.25f + x+y);
  Halide::Image<float_t> input = initial_condition.realize(NX,NY);
  
  Halide::RDom r(0,1000000000);
  Halide::Func f("f");
  f(x,y) = input(x,y);
  
  f(x,y) = f(x,y) * 0.25f + r;

  {
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("input", true, Halide::Float(32)));
    f.compile_to_bitcode("flops.bc", arg_vect, "flops");
    f.compile_to_assembly("flops.s", arg_vect, "flops");
  }

  f.parallel(y).vectorize(x,8);
  


  Halide::Image<float_t> output = f.realize(NX,NY);

  float sum = 0;
  for (int j = 0; j < NY; ++j) {
    for (int i = 0; i < NX; ++i) {
      sum += output(i,j);
    }
  }
  cout << sum << endl;

}
