#include <Halide.h>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <sstream>
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

int N_FUSION = 4;
int CELL2_CHOICE=4; // 0..8
int CELL3_CHOICE=4; // 0..17
int N_VECTOR=8;
int N_UNROLL=4;
int N_TILE_X=64;
int N_TILE_Y=64;

int N_FUSION_0= 4;
int CELL2_CHOICE_0=4; // 0..8
int CELL3_CHOICE_0=4; // 0..17
int N_VECTOR_0=8;
int N_UNROLL_0=4;
int N_TILE_X_0=64;
int N_TILE_Y_0=64;


double bench(bool is_c_gen, int NX, int NY, int MAX_T) {

  Halide::Var x("x"),y("y"), yo("yo"), yi("yi"), xo("xo"), xi("xi");
  Halide::Func initial_condition("initial_condition");
  initial_condition(x, y) = 0.0f;
  initial_condition(NX/3, NY/4) = 1.0f* NX*NY ;
 
  Halide::ImageParam inPar(Halide::Float(32), 2, "inPar"); // 32bit float 2D 

  Halide::Image<float_t> input = initial_condition.realize(NX,NY); 
  Halide::Image<float_t> output(NX,NY); 
  

  // precompile the programs
  const float a = 0.5f, b = 0.25f;
  //std::vector<Halide::Func> cell2(N_FUSION), cell3(N_FUSION);
  Halide::Func cell2[N_FUSION], cell3[N_FUSION];

  for(int i_f = 0; i_f < N_FUSION ; ++i_f) {
    std::cerr << "define cell2-" << i_f << std::endl;
    if (i_f==0) {
      cell2[i_f](x,y)= (a * inPar(x,y) + b * inPar(clamp(x+1,0,NX-1),y) + b * inPar(clamp(x-1,0,NX-1),y)) ;
    } else {
      cell2[i_f](x,y)= (a * cell3[i_f-1](x,y) + b *  cell3[i_f-1](clamp(x+1,0,NX-1),y) + b * cell3[i_f-1](clamp(x-1,0,NX-1),y)) ;
    }

    std::cerr << "define cell3-" << i_f << std::endl;    
    cell3[i_f](x,y)= (a * cell2[i_f](x,y) + b * cell2[i_f](x,clamp(y+1,0,NY-1)) + b * cell2[i_f](x,clamp(y-1,0,NY-1))) ;

  }


  Halide::Var nid("nid");
  for(int i_f = N_FUSION-1; i_f >=0 ; --i_f) {
    if (i_f==N_FUSION-1){
      cell3[i_f].tile(x,y, xo,yo, xi, yi, N_TILE_X, N_TILE_Y).fuse(xo,yo,nid).parallel(nid);
      if (not is_c_gen) cell3[i_f].vectorize(xi,N_VECTOR);
      cell3[i_f].unroll(xi,N_UNROLL);

    }
    else{
      switch(CELL3_CHOICE){
      case 0 : cell3[i_f].compute_root(); break;
      case 1 : cell3[i_f].store_root().compute_at(cell3[N_FUSION-1], Halide::Var::outermost()); break;
      case 2 : cell3[i_f].compute_at(cell3[N_FUSION-1], Halide::Var::outermost()); break;
      case 3 : cell3[i_f].compute_at(cell3[N_FUSION-1], nid); break;
      case 4 : cell3[i_f].store_at(cell3[N_FUSION-1], nid).compute_at(cell3[N_FUSION-1], yi); break;
      case 5 : cell3[i_f].store_at(cell3[N_FUSION-1], nid).compute_at(cell2[N_FUSION-1], Halide::Var::outermost()); break;
      case 6 : cell3[i_f].store_at(cell3[N_FUSION-1], nid).compute_at(cell2[N_FUSION-1], y); break;
      case 7 : cell3[i_f].store_at(cell3[N_FUSION-1], nid).compute_at(cell2[N_FUSION-1], x); break;
      case 8 : cell3[i_f].compute_at(cell3[N_FUSION-1], yi); break;
      case 9 : cell3[i_f].store_at(cell3[N_FUSION-1], yi).compute_at(cell2[N_FUSION-1], Halide::Var::outermost()); break;
      case 10 : cell3[i_f].store_at(cell3[N_FUSION-1], yi).compute_at(cell2[N_FUSION-1], y); break;
      case 11 : cell3[i_f].store_at(cell3[N_FUSION-1], yi).compute_at(cell2[N_FUSION-1], x); break;
      case 12 : cell3[i_f].compute_at(cell2[N_FUSION-1], Halide::Var::outermost()); break;
      case 13 : cell3[i_f].store_at(cell2[N_FUSION-1], Halide::Var::outermost()).compute_at(cell2[N_FUSION-1], y); break;
      case 14 : cell3[i_f].store_at(cell2[N_FUSION-1], Halide::Var::outermost()).compute_at(cell2[N_FUSION-1], x); break;
      case 15 : cell3[i_f].compute_at(cell2[N_FUSION-1], y); break;
      case 16 : cell3[i_f].store_at(cell2[N_FUSION-1], y).compute_at(cell2[N_FUSION-1], x); break;
      default : cell3[i_f].compute_at(cell2[N_FUSION-1], x); break;
      }
      if (not is_c_gen) cell3[i_f].vectorize(x,N_VECTOR);
      cell3[i_f].unroll(x,N_UNROLL);

    }


    switch(CELL2_CHOICE) {
    case 0 : cell2[i_f].compute_root(); break; 
    case 1 : cell2[i_f].store_root().compute_at(cell3[N_FUSION-1], Halide::Var::outermost()); break; 
    case 2 : cell2[i_f].compute_at(cell3[N_FUSION-1], Halide::Var::outermost()); break; 
    case 3 : cell2[i_f].compute_at(cell3[N_FUSION-1], nid); break; 
    case 4 : cell2[i_f].store_at(cell3[N_FUSION-1], nid).compute_at(cell3[N_FUSION-1], yi); break; 
    case 5 : cell2[i_f].store_at(cell3[N_FUSION-1], nid).compute_at(cell3[N_FUSION-1], xi); break; 
    case 6 : cell2[i_f].compute_at(cell3[N_FUSION-1], yi); break; 
    case 7 : cell2[i_f].store_at(cell3[N_FUSION-1], yi).compute_at(cell3[N_FUSION-1], xi); break; 
    default: cell2[i_f].compute_at(cell3[N_FUSION-1], xi); break; 
    }
    
    if (not is_c_gen) cell2[i_f].vectorize(x,N_VECTOR);
    cell2[i_f].unroll(x,N_UNROLL);

  }


  std::cerr << "all strategy set"<< std::endl;    
  

  // run the computation once to generate the program
  inPar.set(input);
  output=cell3[N_FUSION-1].realize(NX,NY);
  std::swap(input, output);
  if(is_c_gen){
    std::vector<Halide::Argument> arg_vect;
    arg_vect.push_back(Halide::Argument("inPar", true, Halide::Int(32)));
    cell3[N_FUSION-1].compile_to_c("blur-fusion-gen.c", arg_vect, "main_compute");
  }


  double t0 = wtime();
  
  for (int t=0; t<=MAX_T; ++t) {
    // updating logic
    inPar.set(input);
    output=cell3[N_FUSION-1].realize(NX,NY);
    // swap the double-buffer
    std::swap(input, output);
  }

  double t1 = wtime();
  
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

  return t1-t0;
}


int irand(int hi){
  return (rand()/(RAND_MAX/hi))%hi;
}

void reset_params(){
  
  N_FUSION =         N_FUSION_0 ;		   
  CELL2_CHOICE =     CELL2_CHOICE_0 ;	   
  CELL3_CHOICE =     CELL3_CHOICE_0 ;	   
  N_VECTOR = 	       N_VECTOR_0 ; 	   
  N_UNROLL = 	       N_UNROLL_0 ; 	   
  N_TILE_X = 	       N_TILE_X_0 ; 	   
  N_TILE_Y =         N_TILE_Y_0 ;           
}



int main2(){

  size_t nx = 1024;
  size_t ny = 1024;

  double deltaT = bench(true,nx,ny,1);
  if (deltaT > 20) return 0;
  

  {
    ostringstream ossfn;
    ossfn << "result/C"
	  << "_f" << N_FUSION 
	  << "_c" << CELL2_CHOICE
	  << "_d" << CELL3_CHOICE
	  << "_v" << N_VECTOR
	  << "_u" << N_UNROLL
	  << "_tx"<<  N_TILE_X
	  << "_ty"<<  N_TILE_Y << ".txt";
    ofstream ofs(ossfn.str().c_str(), std::ofstream::out | std::ofstream::app);

    for (nx=1024;nx<=4096;nx*=2){
      ny = nx;
      for (size_t t_max=256; t_max < 10000; t_max*=2) {
	if(nx>1024)t_max=8192;
	ostringstream msg;
	deltaT = bench(false, nx,ny,t_max/N_FUSION);
	double num_flop =  double(nx) * double(ny) * double(t_max) * 10 ;
	msg << (num_flop / deltaT/1e9) <<  " GFlops " ;
	msg << nx << " " << ny << " " ;
	msg << t_max << " " << deltaT << "\t";
	msg << " " << N_FUSION 
	    << " " << CELL2_CHOICE
	    << " " << CELL3_CHOICE
	    << " " << N_VECTOR
	    << " " << N_UNROLL
	    << " "<<  N_TILE_X
	    << " "<<  N_TILE_Y;
	
	cerr << msg.str() << endl;
	ofs << msg.str() << endl;
      }
    }
  }
}

int main(int argc, char **argv) {
  srand(time(NULL));
  for(;;) {
    N_FUSION_0=1<<irand(6);
    CELL2_CHOICE_0=irand(9);
    CELL3_CHOICE_0=irand(18);
    N_VECTOR_0	=1<<irand(6);
    N_UNROLL_0	=1<<irand(7);
    N_TILE_X_0	=1<<irand(10);
    N_TILE_Y_0    =1<<irand(10);


    reset_params();    for (N_VECTOR	=1;N_VECTOR    <32  ;N_VECTOR    *=2)  main2();
    reset_params();    for (N_UNROLL	=1;N_UNROLL    <65  ;N_UNROLL    *=2)  main2();
    int nvu = N_VECTOR * N_UNROLL;
    reset_params();    for (N_TILE_X  =nvu;N_TILE_X    <1024;N_TILE_X    *=2)  main2();
    reset_params();    for (N_TILE_Y    =1;N_TILE_Y    <1024;N_TILE_Y    *=2)  main2();
    reset_params();    for (N_FUSION	=1;N_FUSION    <=8  ;N_FUSION    *=2)  main2();
  }
}
