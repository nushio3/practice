#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <stdlib.h>

const int NX=800,NY=600;
const int MAX_T=1000;
 
int main(int argc, char **argv) {
  Halide::Var x,y;
  Halide::Func initial_condition("initial_condition");
  initial_condition(x, y) = 0.0f;
  initial_condition(NX/3, NY/4) = 1.0f* NX*NY ;
 
  Halide::ImageParam inBuf(Halide::Float(32), 2); // 32bit float 2D 
  Halide::ImageParam outBuf(Halide::Float(32), 2); // 32bit float 2D

  Halide::Image<float_t> output = initial_condition.realize(NX,NY); 
  

  inBuf.set(output);
  int ret = system("mkdir -p frame");

  for (int t=0; t<=MAX_T; ++t) {
    // output the current state
    if(t%10==0){
      std::cerr<< t << " " << ret << std::endl;
      std::ofstream ofs("debug.txt");
      Halide::Func dump;
      dump(x,y)=inBuf(x,y);
      Halide::Image<float_t> dumpout = dump.realize(NX,NY); 
      for (int j = 0; j < NY; j++) {
        for (int i = 0; i < NX; i++) {
          ofs <<  i << " " << j << " " << dumpout(i, j) << std::endl;
        }
        ofs << std::endl;
      }
      ofs.close();
      ret = system("gnuplot plot.gnu");
      char cmd[256];
      sprintf(cmd, "mv debug.png frame/%04d.png",t);
      ret += system(cmd);

    } 
    


    // updating logic
    Halide::Func cell2, cell3;
    cell2(x,y)= (inBuf(x,y) + inBuf((x+1)%NX,y) + inBuf((x+NX-1)%NX,y))/3 ;
    cell3(x,y)= (cell2(x,y) + cell2(x,(y+1)%NY) + cell2(x,(y+NY-1)%NY))/3 ;
    Halide::Var yo, yi;
    cell3.split(y,yo,yi,16);
    cell3.parallel(yo);
    cell2.store_at(cell3,yo);
    cell2.compute_at(cell3,yi);
    cell2.vectorize(x,4);
    output=cell3.realize(NX,NY);
    outBuf.set(output);

    // swap the double-buffer
    std::swap(inBuf, outBuf);
  }
  return 0;
}

