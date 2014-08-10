#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_wavelet2d.h>

void my_wavelet(double *data, int tda, int width, int height, double *work) {
  if(width <=1) return;
  for(int y =0; y < height; ++y) {
    for(int x =0; x < width; ++x) {
      work[x]=data[y*tda+x];
    }
    for(int x =0; x < width/2; ++x) {
      data[y*tda+x]=work[2*x]+work[2*x+1];
      data[y*tda+width/2+x]=work[2*x]-work[2*x+1];
    }
  }
  for(int x =0; x < width; ++x) {
    for(int y =0; y < height; ++y) {
      work[y]=data[y*tda+x];
    }
    for(int y =0; y < height/2; ++y) {
      data[y*tda+x]=work[2*y]+work[2*y+1];
      data[(y+height/2)*tda+x]=work[2*y]-work[2*y+1];
    }
  }
  for(int x =0; x < width; ++x) {
    for(int y =0; y < height; ++y) {
      data[y*tda+x]/=2;
    }
  }
  my_wavelet(data,tda,width/2,height/2,work);
}

int
main (int argc, char **argv)
{
  int x,y, width = 8, height = 8;
  int n = width * height;
  double *data = (double*)malloc (n * sizeof (double));
  double *data2 = (double*)malloc (n * sizeof (double));
  double *abscoeff = (double*)malloc (n * sizeof (double));
  size_t *p = (size_t*)malloc (n * sizeof (size_t));

  FILE * f;
  gsl_wavelet *w;
  gsl_wavelet_workspace *work;

  srand(time(NULL));

  w = gsl_wavelet_alloc (gsl_wavelet_haar, 2);
  work = gsl_wavelet_workspace_alloc (n);

  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      data[y*width+x] = rand()/double(RAND_MAX)*10.0;
      data2[y*width+x] = data[y*width+x] ;
    }
  }

  printf("original matrix\n");
  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      printf ("%5f\t", data[y*width+x]);
    }
    printf("\n");
  }  
  printf("\n");

  gsl_wavelet2d_nstransform_forward (w, data, width,width,height , work);
  gsl_wavelet_free (w);
  gsl_wavelet_workspace_free (work);

  printf("wavelet by gsl\n");
  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      printf ("%5f\t", data[y*width+x]);
    }
    printf("\n");
  }  
  printf("\n");

  my_wavelet(data2,width,width,height,(double*)work);

  printf("wavelet by me\n");
  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      printf ("%5f\t", data2[y*width+x]);
    }
    printf("\n");
  }  
  printf("\n");


  free (data);
  free (data2);
  free (abscoeff);
  free (p);
  return 0;
}
