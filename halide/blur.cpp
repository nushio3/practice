#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <stdlib.h>

const int NX=1200,NY=800;
const int MAX_T=400;
 
int main(int argc, char **argv) {
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
  //cell2.compute_root();
  // Halide::Var yo, yi;
  // cell3.split(y,yo,yi,16);
  // cell3.parallel(yo);
  // //  cell3.vectorize(x,4);
  // cell2.store_at(cell3,yo);
  // cell2.compute_at(cell3,yi);
  // cell2.vectorize(x,4);
  
  {
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("inPar", true, Halide::Int(32)));
    cell3.compile_to_bitcode("blur.bc", arg_vect, "blur");
  }

  
  std::cerr<< "ready to blur!" << std::endl;

  int ret = system("mkdir -p frame");

  for (int t=0; t<=MAX_T; ++t) {
    // output the current state
    if(t%100==0){
      std::cerr<< t << " " << ret << std::endl;
      std::ofstream ofs("debug.txt");
      for (int j = 0; j < NY; j++) {
        for (int i = 0; i < NX; i++) {
          ofs <<  i << " " << j << " " << input(i, j) << std::endl;
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
    inPar.set(input);
    output=cell3.realize(NX,NY);

    // swap the double-buffer
    std::swap(input, output);
  }
  return 0;
}

