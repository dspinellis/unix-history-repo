#include "longlong.h"

long long
__divdi3 (u, v)
     long long u, v;
{
  if (u < 0)
    if (v < 0)
      return (unsigned long long) -u / (unsigned long long) -v;
    else
      return - ((unsigned long long) -u / (unsigned long long) v);
  else
    if (v < 0)
      return - ((unsigned long long) u / (unsigned long long) -v);
    else
      return (unsigned long long) u / (unsigned long long) v;
}
