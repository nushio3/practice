#include <Halide.h>
#include <stdio.h>
#include <vector>

 
const int NX=5,NY=4;
const int MAX_T=4;
 
int main(int argc, char **argv) {
  printf("Cyclic Index Test\n");
 
  Halide::Var x,y;
  Halide::Func cdb("cell-debug");
  cdb(x,y) = ((x+1)%NX)*100 + (y+1)%NY;
  Halide::Image<int32_t> odb = cdb.realize(NX,NY); 
  for (int j = 0; j < NY; j++) {
    for (int i = 0; i < NX; i++) {
      printf("%8d", odb(i, j));
    }
    printf("\n");
  }


  {
    printf("C++ Version\n");
    std::vector<std::vector<int> > inBuf(NY, std::vector<int>(NX));
    std::vector<std::vector<int> > outBuf(NY, std::vector<int>(NX));
    for(int j=0; j<NY; ++j) {
      for(int i=0; i<NX; ++i) {
	inBuf[j][i] = i+j;
      }
    }
    for (int t=0;t<MAX_T;++t) {
      for (int j = 0; j < NY; j++) {
	for (int i = 0; i < NX; i++) {
	  printf("%8d", inBuf[j][i]);
	}
	printf("\n");
      }
      printf("\n");
      
      for (int j = 0; j < NY; j++) {
	for (int i = 0; i < NX; i++) {
	  outBuf[j][i] = 100*inBuf[j][i]
	    + inBuf[(j+1)%NY][(i+1)%NX];
	}
      }
      std::swap(inBuf, outBuf);
      

    }
  }



  {
    printf("Halide alloc Version\n");
    Halide::Func cell("cell");
    cell(x, y) = x+y;

    Halide::ImageParam inBuf(Halide::Int(32), 2, "inBuf"); // int32_t 2D

    Halide::Func cell2;
    cell2(x,y)=100*inBuf(x,y)+inBuf((x+1)%NX,(y+1)%NY);

    Halide::Image<int32_t> input = cell.realize(NX,NY); 
    Halide::Image<int32_t> output(NX, NY);
  
    for (int t=0; t<MAX_T; ++t) {
      // output the current state
      for (int j = 0; j < NY; j++) {
	for (int i = 0; i < NX; i++) {
	  printf("%8d", input(i, j));
	}
	printf("\n");
      }
      printf("\n");
 
      // updating logic
      inBuf.set(input);

      {
        std::vector<Halide::Argument> arg_vect;
        arg_vect.push_back(Halide::Argument("inBuf", true, Halide::Int(32)));
        cell2.compile_to_bitcode("double-buffer.bc", arg_vect, "double_buffer");
      }

      output = cell2.realize(NX,NY);

      // swap the double-buffer
      std::swap(input, output);
    }
  } 


  return 0;
}

