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
   switch (cvnum(&arg1, &n)) {
      case T_LONGINT:
	 l = -n.i;
         if (n.i < 0 && l < 0)
            runerr(203, &arg1);
         mkint(l, &arg0);
         break;

      case T_REAL:
	 mkreal(-n.r, &arg0);
	 break;

      default:
	 runerr(102, &arg1);
      }
   ClearBound;
   }
struct b_iproc Bneg = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(neg),
   1,
   -1,
   0,
   0,
   {1, "-"}
   };
