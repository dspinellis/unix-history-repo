#include "../h/rt.h"

/*
 * +x - convert x to numeric type.
 */

number(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   union numeric n;

   SetBound;
   switch (cvnum(&arg1, &n)) {
      case T_LONGINT:
	 mkint(n.i, &arg0);
	 break;

      case T_REAL:
         mkreal(n.r, &arg0);
	 break;

      default:
         runerr(102, &arg1);
      }
   ClearBound;
   }
struct b_iproc Bnumber = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(number),
   1,
   -1,
   0,
   0,
   {1, "+"}
   };
