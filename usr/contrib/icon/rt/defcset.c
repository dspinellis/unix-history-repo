#include "../h/rt.h"

/*
 * defcset(dp,cp,buf,def) - if dp is null, default to def;
 *  otherwise, convert to cset or die trying.
 */

defcset(dp, cp, buf, def)
struct descrip *dp;
int **cp;
int *buf, *def;
   {
   DeRef(*dp)
   if (NULLDESC(*dp)) {
      *cp = def;
      return 1;
      }
   if (cvcset(dp, cp, buf) == NULL)
      runerr(104, dp);
   return 0;
   }
