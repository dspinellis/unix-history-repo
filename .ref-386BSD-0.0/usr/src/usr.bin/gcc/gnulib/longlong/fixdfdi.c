#include "longlong.h"

long long
__fixdfdi (a)
     double a;
{
  long long __fixunsdfdi (double a);

  if (a < 0)
    return - __fixunsdfdi (-a);
  return __fixunsdfdi (a);
}
