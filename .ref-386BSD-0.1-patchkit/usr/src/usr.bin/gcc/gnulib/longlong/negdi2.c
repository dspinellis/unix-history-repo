#include "longlong.h"

static int bneg ();

long long 
__negdi2 (u)
     long long u;
{
  unsigned long a[2], b[2];
  long_long w;
  long_long uu;

  uu.ll = u;

  a[HIGH] = uu.s.high;
  a[LOW] = uu.s.low;

  bneg (a, b, sizeof b);

  w.s.high = b[HIGH];
  w.s.low = b[LOW];
  return w.ll;
}

static int
bneg (a, b, n)
     unsigned short *a, *b;
     size_t n;
{
  signed long acc;
  int i;

  n /= sizeof (short);

  acc = 0;
  for (i = little_end (n); is_not_msd (i, n); i = next_msd (i))
    {
      acc -= a[i];
      b[i] = acc & low16;
      acc = acc >> 16;
    }
  return acc;
}
