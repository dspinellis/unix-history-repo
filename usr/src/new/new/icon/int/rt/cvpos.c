#include "../h/rt.h"

/*
 * cvpos - convert position to strictly positive position
 * given string or list length.
 */

cvpos(pos, len)
long pos;
register int len;
   {
   DclSave
   register int p;

   if ((long)(p = pos) != pos)
      fail();
   if (p < -len || p > len + 1)
      fail();
   if (pos > 0)
      return (p);
   return (len + p + 1);
   }
