#include "../h/rt.h"

/*
 * limit - explicit limitation initialization.
 */

limit(nargs, arg0)
int nargs;
struct descrip arg0;
   {
   DclSave
   long l;

   SetBound;

   /*
    * The limit is both passed and returned in arg0.  The limit must
    *  be an integer.  If the limit is 0, the expression being evaluated
    *  fails.  If the limit is < 0, it is an error.  Note that the
    *  result produced by limit is ultimately picked up by the lsusp
    *  function.
    */
   switch (cvint(&arg0, &l)) {
      case T_INTEGER:  mkint(l, &arg0); break;
#ifdef LONGS
      case T_LONGINT:  runerr(205, &arg0);
#endif LONGS
      default:         runerr(101, &arg0);
      }

   if (l < 0)
      runerr(205, &arg0);
   if (l == 0)
      fail();
   ClearBound;
   }
