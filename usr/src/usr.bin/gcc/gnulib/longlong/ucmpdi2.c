#include "longlong.h"

SItype
__ucmpdi2 (a, b)
     long long a, b;
{
  long_long au, bu;

  au.ll = a, bu.ll = b;

  if ((unsigned) au.s.high < (unsigned) bu.s.high)
    return 0;
  else if ((unsigned) au.s.high > (unsigned) bu.s.high)
    return 2;
  if ((unsigned) au.s.low < (unsigned) bu.s.low)
    return 0;
  else if ((unsigned) au.s.low > (unsigned) bu.s.low)
    return 2;
  return 1;
}
