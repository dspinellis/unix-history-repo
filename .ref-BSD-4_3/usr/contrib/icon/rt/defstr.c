#include "../h/rt.h"

/*
 * defstr - if dp is null, default to def; otherwise, convert to string.
 *  *dp gets a descriptor for the resulting string.  buf is used as
 *  a scratch buffer for the conversion (if necessary).
 */

defstr(dp, buf, def)
struct descrip *dp;
char *buf;
struct descrip *def;
   {
   DeRef(*dp)
   if (NULLDESC(*dp)) {
      *dp = *def;
      return 1;
      }
   if (cvstr(dp, buf) == NULL)
      runerr(103, dp);
   return 0;
   }
