#include <iostream>
#include <cmath>

using namespace std;

const int NX = 100;
const int NY = 100;
const int NZ = 100;

const double Dt = 0.1;
const double Dx = 0.1;
const double Dy = 0.1;
const double Dz = 0.1;

double Sxx[NZ][NY][NX];
double Syy[NZ][NY][NX];
double Szz[NZ][NY][NX];
double Sxy[NZ][NY][NX];
double Sxz[NZ][NY][NX];
double Syz[NZ][NY][NX];

double Vx[NZ][NY][NX];
double Vy[NZ][NY][NX];
double Vz[NZ][NY][NX];

double dSxx_dx[NZ][NY][NX];
double dSxz_dx[NZ][NY][NX];
double dSxy_dx[NZ][NY][NX];
double dSyy_dy[NZ][NY][NX];
double dSxy_dy[NZ][NY][NX];
double dSyz_dy[NZ][NY][NX];
double dSzz_dz[NZ][NY][NX];
double dSxz_dz[NZ][NY][NX];
double dSyz_dz[NZ][NY][NX];


double dVx_dx[NZ][NY][NX];
double dVy_dx[NZ][NY][NX];
double dVz_dx[NZ][NY][NX];
double dVx_dy[NZ][NY][NX];
double dVy_dy[NZ][NY][NX];
double dVz_dy[NZ][NY][NX];
double dVx_dz[NZ][NY][NX];
double dVy_dz[NZ][NY][NX];
double dVz_dz[NZ][NY][NX];

const double r40 = 9.0/8.0;
const double r41 = 1.0/24.0;



template <class T> T sq(const T &x) { return x*x; }

int init() {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	Sxx[k][j][i] = 0;
	Syy[k][j][i] = 0;
	Szz[k][j][i] = 0;
	Syz[k][j][i] = 0;
	Sxz[k][j][i] = 0;
	Sxy[k][j][i] = 0;

	Vx[k][j][i] = 0;
	Vy[k][j][i] = 0;
	Vz[k][j][i] = 0.1 * exp( (sq(i-0.5*NX) + sq(j-0.5*NY) + sq(k-0.5*NZ)) 
				 / 100);
      }
    }
  }

}

void diffx3_m4(double f[NZ][NY][NX], double df_dx[NZ][NY][NX] ) {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	df_dx[k][j][i]
	  = ( (f[k][j][i  ] - f[k][j][i-1] ) * r40 
	    - (f[k][j][i+1] - f[k][j][i-2] ) * r41) / Dx;
      }
    }
  }  
}

void diffx3_p4(double f[NZ][NY][NX], double df_dx[NZ][NY][NX] ) {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	df_dx[k][j][i]
	  = ( (f[k][j][i+1] - f[k][j][i  ] ) * r40 
	    - (f[k][j][i+2] - f[k][j][i-1] ) * r41) / Dx;
      }
    }
  }  
}

void diffy3_m4(double f[NZ][NY][NX], double df_dy[NZ][NY][NX] ) {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	df_dy[k][j][i]
	  = ( (f[k][j  ][i] - f[k][j-1][i] ) * r40 
	    - (f[k][j+1][i] - f[k][j-2][i] ) * r41) / Dy;
      }
    }
  }  
}

void diffy3_p4(double f[NZ][NY][NX], double df_dy[NZ][NY][NX] ) {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	df_dy[k][j][i]
	  = ( (f[k][j+1][i] - f[k][j  ][i] ) * r40 
	    - (f[k][j+2][i] - f[k][j-1][i] ) * r41) / Dy;
      }
    }
  }  
}

void diffz3_m4(double f[NZ][NY][NX], double df_dz[NZ][NY][NX] ) {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	df_dz[k][j][i]
	  = ( (f[k  ][j][i] - f[k-1][j][i] ) * r40 
	    - (f[k+1][j][i] - f[k-2][j][i] ) * r41) / Dz;
      }
    }
  }  
}

void diffz3_p4(double f[NZ][NY][NX], double df_dz[NZ][NY][NX] ) {
  for (int k = 0; k < NZ; ++k) {
    for (int j = 0; j < NY; ++j) {
      for (int i = 0; i < NX; ++i) {
	df_dz[k][j][i]
	  = ( (f[k+1][j][i] - f[k  ][j][i] ) * r40 
	    - (f[k+2][j][i] - f[k-1][j][i] ) * r41) / Dz;
      }
    }
  }  
}

int diff_V() {
  diffx3_m4( Vx, dVx_dx );
  diffy3_p4( Vx, dVx_dy );
  diffz3_p4( Vx, dVx_dz );
  diffx3_p4( Vy, dVy_dx );
  diffy3_m4( Vy, dVy_dy );
  diffz3_p4( Vy, dVy_dz );
  diffx3_p4( Vz, dVz_dx );
  diffy3_p4( Vz, dVz_dy );
  diffz3_m4( Vz, dVz_dz );
}

int diff_S() {
  diffx3_p4( Sxx, dSxx_dx );
  diffy3_p4( Syy, dSyy_dy );
  diffx3_m4( Sxy, dSxy_dx );
  diffx3_m4( Sxz, dSxz_dx );
  diffy3_m4( Sxy, dSxy_dy );
  diffy3_m4( Syz, dSyz_dy );
  diffz3_p4( Szz, dSzz_dz );
  diffz3_m4( Sxz, dSxz_dz );
  diffz3_m4( Syz, dSyz_dz );
}


int main () {
  cout << "hello" << endl;
  return 0;
}
