/* 
Copyright (C) 1990 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

#ifdef __GNUG__
#pragma implementation
#endif
#include <builtin.h>
#include <math.h>

double pow(double x, long p)
{
  if (p == 0)
    return 1.0;
  else if (x == 0.0)
    return 0.0;
  else
  {
    if (p < 0)
    {
      p = -p;
      x = 1.0 / x;
    }

    double r = 1.0;
    for(;;)
    {
      if (p & 1)
        r *= x;
      if ((p >>= 1) == 0)
        return r;
      else
        x *= x;
    }
  }
}

long  pow(long  x, long p)
{
  if (p == 0)
    return 1;
  else if (p < 0 || x == 0)
    return 0;
  else
  {
    long r = 1;
    for(;;)
    {
      if (p & 1)
        r *= x;
      if ((p >>= 1) == 0)
        return r;
      else
        x *= x;
    }
  }
}
