#include "longlong.h"

long long
__one_cmpldi2 (u)
     long long u;
{
  long_long w;
  long_long uu;

  uu.ll = u;

  w.s.high = ~uu.s.high;
  w.s.low = ~uu.s.low;

  return w.ll;
}
