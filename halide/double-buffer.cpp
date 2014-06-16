#include <Halide.h>
#include <stdio.h>
#include <vector>
 
const int NX=5,NY=4;
 
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
    for (int t=0;t<3;++t) {
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
 
    Halide::ImageParam inBuf(Halide::Int(32), 2); // int32_t 2D
    Halide::ImageParam outBuf(Halide::Int(32), 2); // int32_t 2D

    Halide::Image<int32_t> output = cell.realize(NX,NY); 
  

    inBuf.set(output);

    for (int t=0; t<3; ++t) {
      // output the current state
      Halide::Func dump;
      dump(x,y)=inBuf(x,y);
      Halide::Image<int32_t> dumpout = dump.realize(NX,NY); 
      for (int j = 0; j < NY; j++) {
	for (int i = 0; i < NX; i++) {
	  printf("%8d", dumpout(i, j));
	}
	printf("\n");
      }
      printf("\n");
 
      // updating logic
      Halide::Func cell2;
      cell2(x,y)=100*inBuf(x,y)+inBuf((x+1)%NX,(y+1)%NY);
      output=cell2.realize(NX,NY);
      outBuf.set(output);

      // swap the double-buffer
      std::swap(inBuf, outBuf);
    }
  } 


  return 0;
}

