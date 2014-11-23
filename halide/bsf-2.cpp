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

int N_FUSION = 3;
int CELL2_CHOICE=0; // 0..8
int CELL3_CHOICE=0; // 0..17
int N_VECTOR=8;
int N_UNROLL=4;
int N_TILE_X=512;
int N_TILE_Y=512;
int N_TILE_XI=32;
int N_TILE_YI=32;

int N_FUSION_0= 6;
int CELL2_CHOICE_0=0; // 0..8
int CELL3_CHOICE_0=0; // 0..17
int N_VECTOR_0=8;
int N_UNROLL_0=4;
int N_TILE_X_0=1024;
int N_TILE_Y_0=128;
int N_TILE_XI_0=64;
int N_TILE_YI_0=32;


double bench(bool is_c_gen, int NX, int NY, int MAX_T) {

  Halide::Var x("x"),y("y"), yo("yo"), yi("yi"), xo("xo"), xi("xi");
  Halide::Var  yii("yii"), xii("xii");
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
      cell3[i_f].tile(xi,yi, xii, yii, N_TILE_XI, N_TILE_YI);
      if (not is_c_gen) cell3[i_f].vectorize(xii,N_VECTOR);
      cell3[i_f].unroll(xii,N_UNROLL);

    }
    else{
      //cell3[i_f].tile(x,y, xo,yo, xi, yi, N_TILE_X, N_TILE_Y).fuse(xo,yo,nid).parallel(nid);
      cell3[i_f].store_at(cell3[N_FUSION-1], xi).compute_at(cell3[N_FUSION-1], xii);
      if (not is_c_gen) cell3[i_f].vectorize(x,N_VECTOR);
      cell3[i_f].unroll(x,N_UNROLL);

    }


    //cell2[i_f].tile(x,y, xo,yo, xi, yi, N_TILE_X, N_TILE_Y).fuse(xo,yo,nid).parallel(nid);    
    cell2[i_f].store_at(cell3[N_FUSION-1], xi).compute_at(cell3[N_FUSION-1], xii);

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
    std::ostringstream fn_str;
    fn_str << "generated-bsf-NF_" << N_FUSION << ".c";
    
    cell3[N_FUSION-1].compile_to_c(fn_str.str().c_str(), arg_vect, "main_compute");
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

  size_t nx = 2048;
  size_t ny = 2048;

  double deltaT = bench(false,nx,ny,1);
  if (deltaT > 20) return 0;
  

  {
    ostringstream ossfn;
    ossfn << "result/E"
	  << "_f" << N_FUSION 
	  << "_c" << CELL2_CHOICE
	  << "_d" << CELL3_CHOICE
	  << "_v" << N_VECTOR
	  << "_u" << N_UNROLL
	  << "_tx"<<  N_TILE_X
	  << "_ty"<<  N_TILE_Y 
	  << "_ix"<<  N_TILE_XI
	  << "_iy"<<  N_TILE_YI << ".txt";
    ofstream ofs(ossfn.str().c_str(), std::ofstream::out | std::ofstream::app);

    for (nx=2048;nx<=8192;nx*=2){
      ny = nx;
      for (size_t t_max=256; t_max < 1030; t_max*=2) {
	t_max=1024;
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
	    << " "<<  N_TILE_Y
	    << " "<<  N_TILE_XI
	    << " "<<  N_TILE_YI;
	
	cerr << msg.str() << endl;
	ofs << msg.str() << endl;
      }
    }
  }
  
}

int main(int argc, char **argv) {
  srand(time(NULL));
  bench(true,8192,8192,1024);
  for(;;) {
    N_TILE_X=1<<irand(11);
    N_TILE_Y=1<<irand(11);
    N_TILE_XI=1<<irand(10);
    N_TILE_YI=1<<irand(10);
    N_UNROLL=1+irand(16);
    N_FUSION=1+irand(8);
    if(N_TILE_X < N_VECTOR*N_UNROLL) continue;
    if(N_TILE_XI >= N_TILE_X) continue;
    if(N_TILE_YI >= N_TILE_Y) continue;
    main2();
  }
}
