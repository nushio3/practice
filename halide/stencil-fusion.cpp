#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <stdlib.h>

using namespace std;

const int NX=10;
 
void prog01()
{
  Halide::Var x;
  Halide::Func init_cond;
  init_cond(x) = 1.0f*x;
  Halide::Image<float_t> input, output;
  input=init_cond.realize(NX);

  Halide::ImageParam inPar(Halide::Float(32), 1, "inPar");
  Halide::Func cell;
  cell(x)=inPar(x)+1;
  
  {
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("inPar", true, Halide::Int(32)));
    cell.compile_to_bitcode("stencil-fusion-01.bc", arg_vect, "blur");
  }


  for (int t =0; t < 100000; ++t) {
    inPar.set(input);
    output = cell.realize(NX);
    swap(output,input);
  }

  for (int i = 0; i < NX; ++i) 
    cout << input(i) << " ";
  cout << endl;
}

void prog02()
{
  Halide::Var x;
  Halide::Func init_cond;
  init_cond(x) = 1.0f*x;
  Halide::Image<float_t> input, output;
  input=init_cond.realize(NX);

  Halide::ImageParam inPar(Halide::Float(32), 1, "inPar");
  Halide::Func cell0,cell1,cell2,cell3;
  cell0(x)=inPar(x)+1;
  cell1(x)=cell0(x)+1;
  cell2(x)=cell1(x)+1;
  cell3(x)=cell2(x)+1;
  
  {
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("inPar", true, Halide::Int(32)));
    cell3.compile_to_bitcode("stencil-fusion-02.bc", arg_vect, "blur");
  }


  for (int t =0; t < 25000; ++t) {
    inPar.set(input);
    output = cell3.realize(NX);
    swap(output,input);
  }

  for (int i = 0; i < NX; ++i) 
    cout << input(i) << " ";
  cout << endl;
}


void prog03()
{
  Halide::Var x;
  Halide::Func init_cond;
  init_cond(x) = 1.0f*x;
  Halide::Image<float_t> input, output;
  input=init_cond.realize(NX);

  Halide::ImageParam inPar(Halide::Float(32), 1, "inPar");
  Halide::Func cell[4];
  cell[0](x)=inPar(x)+1;
  cell[1](x)=cell[0](x)+1;
  cell[2](x)=cell[1](x)+1;
  cell[3](x)=cell[2](x)+1;
  
  {
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("inPar", true, Halide::Int(32)));
    cell[3].compile_to_bitcode("stencil-fusion-02.bc", arg_vect, "blur");
  }


  for (int t =0; t < 25000; ++t) {
    inPar.set(input);
    output = cell[3].realize(NX);
    swap(output,input);
  }

  for (int i = 0; i < NX; ++i) 
    cout << input(i) << " ";
  cout << endl;
}


void prog04()
{
  Halide::Var x;
  Halide::Func init_cond;
  init_cond(x) = 1.0f*x;
  Halide::Image<float_t> input, output;
  input=init_cond.realize(NX);

  Halide::ImageParam inPar(Halide::Float(32), 1, "inPar");
  vector<Halide::Func> cell(4);
  cell[0](x)=inPar(x)+1;
  cell[1](x)=cell[0](x)+1;
  cell[2](x)=cell[1](x)+1;
  cell[3](x)=cell[2](x)+1;
  
  {
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("inPar", true, Halide::Int(32)));
    cell[3].compile_to_bitcode("stencil-fusion-02.bc", arg_vect, "blur");
  }


  for (int t =0; t < 25000; ++t) {
    inPar.set(input);
    output = cell[3].realize(NX);
    swap(output,input);
  }

  for (int i = 0; i < NX; ++i) 
    cout << input(i) << " ";
  cout << endl;
}




int main(int argc, char **argv) {
  prog01();
  prog02();
  prog03();
  prog04();

}

