#include <iostream>
#include <stdlib.h>
#include <sys/time.h>
#include <cmath>

using namespace std;

namespace{
  double wtime() {
    timeval tv;
    gettimeofday(&tv,NULL);
    return double(tv.tv_sec) + double(tv.tv_usec)*1e-6;
  }
}


template <size_t m>
void measure_fmaps(size_t n) {
  double x =  sin(log(wtime()))+1.1;
  double y =  cos(log(wtime()))+1.1;
  double z =  sin(x+y)+1.1;
  double t0 = wtime();
  /*
    f(x,y,z) = (x*y+z)*0.25+0.25

    if the input is in range 0 <= x <= 2.5
    then the outuput is in range 0 <= x <= 2
    
   */

  for (int i =0; i < n; ++i) {

    // since `m` is a compile-time constant, the following loop gets
    // unrolled by -O3.
    for (int j=0; j< m; ++j) {
      x=(x*y+z)*0.25+0.25; 
      y=(y*z+x)*0.25+0.25;
      z=(z*x+y)*0.25+0.25;
    }
  }
  
  
  double t1 = wtime();
  double num_fma = 6*double(n)*double(m);
  cout << n << " " << m << " " ;
  cout << (x+y+z) << " ";
  cout << num_fma/(t1-t0) << "fmaps" << endl;
}


int main () {
  for (size_t n = 1; ; n*=2) {
    measure_fmaps<4>(n);
    measure_fmaps<8>(n);
    measure_fmaps<16>(n);
    measure_fmaps<32>(n);
    measure_fmaps<64>(n);
  }
}
