#include "../h/rt.h"

/*
 * -x - negate x.
 */

neg(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   union numeric n;
   long l;

   SetBound;
   /*
    * x must be numeric.
    */
   switch (cvnum(&arg1, &n)) {
      case T_LONGINT:
         /*
          * If it's an integer, check for overflow by negating it and
          *  seeing if the negation didn't "work".  Use mkint to
          *  construct the return value.
          */
         l = -n.integer;
         if (n.integer < 0 && l < 0)
            runerr(203, &arg1);
         mkint(l, &arg0);
         break;

      case T_REAL:
         /*
          * x is real, just negate it and use mkreal to construct the
          *  return value.
          */
         mkreal(-n.real, &arg0);
         break;

      default:
         /*
          * x isn't numeric.
          */
         runerr(102, &arg1);
      }
   ClearBound;
   }

Opblock(neg,1,"-")
