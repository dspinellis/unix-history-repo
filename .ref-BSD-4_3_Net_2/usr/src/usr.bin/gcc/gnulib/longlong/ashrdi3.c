#include "longlong.h"

long long
__ashrdi3 (u, b1)
     long long u;
     long long b1;
{
  long_long w;
  unsigned long carries;
  int bm;
  long_long uu;
  int b = b1;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (int) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.high = uu.s.high >> 31; /* just to make w.s.high 1..1 or 0..0 */
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      carries = (unsigned long)uu.s.high << bm;
      w.s.high = uu.s.high >> b;
      w.s.low = ((unsigned long)uu.s.low >> b) | carries;
    }

  return w.ll;
}
