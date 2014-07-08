#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <stdlib.h>
#include <sys/time.h>

namespace{
  double wtime() {
    timeval tv;
    gettimeofday(&tv,NULL);
    return double(tv.tv_sec) + double(tv.tv_usec)*1e-6;
  }
}

void bench(int NX, int NY, int MAX_T) {

  Halide::Var x,y, yo, yi;
  Halide::Func initial_condition("initial_condition");
  initial_condition(x, y) = 0.0f;
  initial_condition(NX/3, NY/4) = 1.0f* NX*NY ;
 
  Halide::ImageParam inPar(Halide::Float(32), 2, "inPar"); // 32bit float 2D 

  Halide::Image<float_t> input = initial_condition.realize(NX,NY); 
  Halide::Image<float_t> output(NX,NY); 
  

  // precompile the programs
  Halide::Func cell2, cell3; const float a = 0.5f, b = 0.25f;
  cell2(x,y)= (a * inPar(x,y) + b * inPar(clamp(x+1,0,NX-1),y) + b * inPar(clamp(x+2,0,NX-1),y)) ;
  cell3(x,y)= (a * cell2(x,y) + b * cell2(x,clamp(y+1,0,NY-1)) + b * cell2(x,clamp(y+2,0,NY-1))) ;
  // cell3.parallel(y);

  cell3.split(y, yo, yi, 16).parallel(yo).parallel(yo).vectorize(x,4);
  cell2.store_at(cell3,yo).compute_at(cell3,yi).vectorize(x,4);
  
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
  using std::cout;
  using std::cerr;
  // uisng std::endl;
  

  for (size_t ny = 16; ny < (2<<15); ny*=2) {
    for (size_t nx = 16; nx < (2<<15); nx*=2) {
      if (nx*ny >= (2<<28)) continue;

      for (size_t t_max = 1; t_max <= 1000;t_max *= 10) {
	
	double t0 = wtime();
	bench(nx,ny,t_max);
	double t1 = wtime();
	cout << nx << " " << ny << " " << t_max << " " << (t1-t0) << std::endl;
      }
    }
  }
}
