#include "../h/rt.h"

/*
 * defshort(dp,cp,buf,def) - if dp is null, default to def;
 * otherwise, convert to short integer.
 */

defshort(dp, def)
struct descrip *dp;
int def;
   {
   long l;

   deref(dp);
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
#ifndef VAX
      case T_LONGINT:
         runerr(205, dp);
#endif
      default:
	 runerr(101, dp);
      }
   return 0;
   }
