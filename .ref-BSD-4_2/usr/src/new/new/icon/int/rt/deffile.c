#include "../h/rt.h"

/*
 * deffile(dp,def) - if dp is null, default to def;
 * otherwise, make sure it's a file.
 */

deffile(dp, def)
struct descrip *dp, *def;
   {
   deref(dp);
   if (NULLDESC(*dp)) {
      *dp = *def;
      return 1;
      }
   if (TYPE(*dp) != T_FILE)
      runerr(105, dp);
   return 0;
   }
