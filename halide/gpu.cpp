// On linux, you can compile and run it like so:
// g++ lesson_01*.cpp -g -I ../include -L ../bin -lHalide -o lesson_01
// export LD_LIBRARY_PATH=./Halide/bin/ HL_DEBUG_CODEGEN=1  HL_JIT_TARGET=ptx

#include <Halide.h>

// We'll also include stdio for printf.
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {

  Func gradient("gradient_gpu");
  Var x("x"), y("y");

  gradient(x, y) = (0.0f + x) * y + 42.195f;
  gradient.gpu_tile(x, y, 4,4);
  
  
  Image<float_t> output = gradient.realize(4, 4);

  return 0;
}
