#include "longlong.h"

#define HIGH_WORD_COEFF (((long long) 1) << BITS_PER_WORD)

long long
__fixunsdfdi (a)
     double a;
{
  double b;
  unsigned long long v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to long long!),
     and shift it into the high word.  */
  v = (unsigned long int) b;
  v <<= BITS_PER_WORD;
  /* Remove high part from the double, leaving the low part as flonum.  */
  a -= (double)v;
  /* Convert that to fixed (but not to long long!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (unsigned long int) (- a);
  else
    v += (unsigned long int) a;
  return v;
}
