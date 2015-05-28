typedef float v4f __attribute__((vector_size(16)));

#include <cstdio>
#include <iostream>
using namespace std;

float x[66];
float y[66];
float z[66];

int main () {
  v4f *px,*py,*pz, *pxa, *pxb, *pxc;
  for (int i=0;i<66;++i) {
    x[i]=i*i;
    y[i]=100;
    z[i]=i;
  }

  px=(v4f*)x;
  py=(v4f*)y;
  pz=(v4f*)z;

  pxa=(v4f*)&(x[0]);
  pxb=(v4f*)&(x[1]);
  pxc=(v4f*)&(x[2]);

  for (int i=0;i<16;++i) {
    // *pz =  *py * *px + *pz;
    *pz = *pxa+*pxb+*pxc;
    ++px; ++py; ++pz, ++pxa, ++pxb, ++pxc;
  }
  for (int i=0;i<64;++i) {
    printf("%f %f %f\n",x[i],y[i],z[i]);
  }
  return 0;
}
