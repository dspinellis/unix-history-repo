#include "../h/rt.h"

/*
 * defint - if dp is null, default to def; otherwise, convert to integer.
 *  Note that *lp gets the value.
 */

defint(dp, lp, def)
struct descrip *dp;
long *lp;
int def;
   {
   DeRef(*dp)
   if (NULLDESC(*dp)) {
      *lp = (long)def;
      return 1;
      }
   if (cvint(dp, lp) == NULL)
      runerr(101, dp);
   return 0;
   }
