#include <iostream>
#include "trace-verify.hpp"

#ifdef TRACE
 typedef traced_float Real;
#else
 typedef float Real;
#endif

Real swap_trick (Real a, Real b) {
  trace("a_at_begin",a);
  trace("b_at_begin",b);

  a = a + b;
  b = a - b;
  a = a - b;

  trace("a_at_end",a);
  trace("b_at_end",b);

  prove_equality("a_at_begin","b_at_end");
  return a-b;
}


int main () {
#ifdef TRACE
  swap_trick(traced_float(),traced_float());
#else
  cout << swap_trick(8,80) << endl;
#endif

}
