#include "../h/rt.h"

/*
 * defstr(dp,buf,def) - if dp is null, default to def;
 * otherwise, convert to string.
 */

defstr(dp, buf, def)
struct descrip *dp;
char *buf;
struct descrip *def;
   {
   deref(dp);
   if (NULLDESC(*dp)) {
      *dp = *def;
      return 1;
      }
   if (cvstr(dp, buf) == NULL)
      runerr(103, dp);
   return 0;
   }
