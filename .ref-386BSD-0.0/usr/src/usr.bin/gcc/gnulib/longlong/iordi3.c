#include "longlong.h"

long long 
__iordi3 (u, v)
     long long u, v;
{
  long_long w;
  long_long uu, vv;

  uu.ll = u;
  vv.ll = v;

  w.s.high = uu.s.high | vv.s.high;
  w.s.low = uu.s.low | vv.s.low;

  return w.ll;
}
