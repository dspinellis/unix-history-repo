#include "../h/rt.h"

/*
 * cvpos - convert position to strictly positive position
 *  given string or list length.
 */

cvpos(pos, len)
long pos;
register int len;
   {
   DclSave
   register int p;

   /*
    * Fail if the position isn't in the range of an int. (?)
    */
   if ((long)(p = pos) != pos)
      fail();
   /*
    * Fail if the position is off either end of the string.
    */
   if (p < -len || p > len + 1)
      fail();
   /*
    * If the position is greater than zero, just return it.  Otherwise,
    *  convert the zero/negative position.
    */
   if (pos > 0)
      return (p);
   return (len + p + 1);
   }
