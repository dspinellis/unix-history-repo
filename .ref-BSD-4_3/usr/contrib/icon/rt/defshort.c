#include "../h/rt.h"

/*
 * defshort - if dp is null, default to def; otherwise, convert to short
 *  integer.  The result is an integer value in *dp.
 */

defshort(dp, def)
struct descrip *dp;
int def;
   {
   long l;

   DeRef(*dp)
   if (NULLDESC(*dp)) {
      dp->type = D_INTEGER;
      INTVAL(*dp) = def;
      return 1;
      }
   switch (cvint(dp, &l)) {
      case T_INTEGER:
         dp->type = D_INTEGER;
         INTVAL(*dp) = (int)l;
         break;
#ifdef LONGS
      case T_LONGINT:
         runerr(205, dp);
#endif LONGS
      default:
         runerr(101, dp);
      }
   return 0;
   }
