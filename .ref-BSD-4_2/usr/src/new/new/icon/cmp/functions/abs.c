#include "../h/rt.h"

/*
 * abs(x) - absolute value of x.
 */

Xabs(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   union numeric result;

   switch (cvnum(&arg1, &result)) {
      case T_LONGINT:
         if (result.i < 0L)
            result.i = -result.i;
	 mkint(result.i, &arg0);
         break;

      case T_REAL:
         if (result.r < 0.0)
   	    result.r = -result.r;
         mkreal(result.r, &arg0);
         break;

      default:
         runerr(102, &arg1);
      }
   }

struct b_iproc Babs = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xabs),
   1,
   -1,
   0,
   0,
   {3, "abs"}
   };
