#include "longlong.h"

static int badd ();

long long 
__adddi3 (u, v)
     long long u, v;
{
  long a[2], b[2], c[2];
  long_long w;
  long_long uu, vv;

  uu.ll = u;
  vv.ll = v;

  a[HIGH] = uu.s.high;
  a[LOW] = uu.s.low;
  b[HIGH] = vv.s.high;
  b[LOW] = vv.s.low;

  badd (a, b, c, sizeof c);

  w.s.high = c[HIGH];
  w.s.low = c[LOW];
  return w.ll;
}

static int 
badd (a, b, c, n)
     unsigned short *a, *b, *c;
     size_t n;
{
  unsigned long acc;
  int i;

  n /= sizeof *c;

  acc = 0;
  for (i = little_end (n); is_not_msd (i, n); i = next_msd (i))
    {
      /* Widen before adding to avoid loss of high bits.  */
      acc += (unsigned long) a[i] + b[i];
      c[i] = acc & low16;
      acc = acc >> 16;
    }
  return acc;
}
