#include <iostream>
#define TRACE
#include "trace-verify.hpp"

typedef traced_float Real;

Real sum (Real a, int n) {
  trace("a_at_begin",a);
  trace("a_plus_n",a+float(n));


  for(int i = 0 ; i < n; ++i) {
    a=a+1.0f;
  }

  trace("a_at_end",a);
  prove_equality("a_at_end","a_plus_n");
  return a;
}


int main () {
  sum(traced_float(),3000);
}
