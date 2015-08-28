#include <stdio.h>

#define N 1024

float A[N][N], B[N][N];

int main() {
  int t,i,j;
  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      A[i][j]=i*j;

    }
  }

  for (t = 0; t < 10000; t++) {
    for (i = 1; i < N-1; i++) {
      for (j = 1; j < N-1; j++) {
        B[i][j] = 0.2f * (A[i][j] + A[i][j-1] + A[i][j+1] + A[i-1][j]  + A[i+1][j]);
      }
    }

    for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
        A[i][j]=B[i][j];
      }
    }
  }

  printf("%f\n", A[N/2][N/2]);
}
