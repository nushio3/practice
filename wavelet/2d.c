#include <stdio.h>
#include <math.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_wavelet2d.h>

int
main (int argc, char **argv)
{
  int x,y, width = 4, height = 4;
  int n = width * height;
  double *data = (double*)malloc (n * sizeof (double));
  double *abscoeff = (double*)malloc (n * sizeof (double));
  size_t *p = (size_t*)malloc (n * sizeof (size_t));

  FILE * f;
  gsl_wavelet *w;
  gsl_wavelet_workspace *work;

  w = gsl_wavelet_alloc (gsl_wavelet_haar, 2);
  work = gsl_wavelet_workspace_alloc (n);

  f = fopen (argv[1], "r");
  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      fscanf(f, "%lg",&data[y*width+x]);
    }
  }
  fclose(f);

  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      printf ("%4g", data[y*width+x]);
    }
    printf("\n");
  }  
  printf("\n");

  gsl_wavelet2d_nstransform_forward (w, data, width,width,height , work);
  gsl_wavelet_free (w);
  gsl_wavelet_workspace_free (work);

  for (y=0;y<height;++y) {
    for (x=0;x<width;++x) {
      printf ("%4g", data[y*width+x]);
    }
    printf("\n");
  }  
  printf("\n");

  free (data);
  free (abscoeff);
  free (p);
  return 0;
}
