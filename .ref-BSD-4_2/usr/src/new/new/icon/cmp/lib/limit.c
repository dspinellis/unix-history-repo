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

   switch (cvint(&arg0, &l)) {
      case T_INTEGER:  mkint(l, &arg0); break;
#ifndef BIT32
      case T_LONGINT:  runerr(205, &arg0);
#endif
      default:         runerr(101, &arg0);
      }

   if (l < 0)
      runerr(205, &arg0);
   if (l == 0)
      fail();
   ClearBound;
   }
