#include "../h/rt.h"

/*
 * defint(dp,lp,def) - if dp is null, default to def;
 * otherwise, convert to integer.
 */

defint(dp, lp, def)
struct descrip *dp;
long *lp;
int def;
   {
   deref(dp);
   if (NULLDESC(*dp)) {
      *lp = (long)def;
      return 1;
      }
   if (cvint(dp, lp) == NULL)
      runerr(101, dp);
   return 0;
   }
