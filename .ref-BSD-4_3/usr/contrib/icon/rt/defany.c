#include "../h/rt.h"

/*
 * defany(dp,def) - if dp is null, default to def.
 */

defany(dp, def)
struct descrip *dp, *def;
   {
   DeRef(*dp)
   if (NULLDESC(*dp)) {
      *dp = *def;
      return 1;
      }
   return 0;
   }
