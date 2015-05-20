typedef float v4f __attribute__((vector_size(32)));

#include <iostream>
using namespace std;

float x[64];
float y[64];
float z[64];

int main () {
  v4f *px,*py,*pz
  for (int i=0;i<16;++i) {
    *pz =  *py * *px;
    ++px; ++py; ++pz;
  }
  return 0;
}
