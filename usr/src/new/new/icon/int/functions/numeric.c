#include "../h/rt.h"

/*
 * numeric(x) - convert x to numeric type.
 */

Xnumeric(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   union numeric n1;

   switch (cvnum(&arg1, &n1)) {
#ifdef  BIT32
      case T_INTEGER:
#else
      case T_LONGINT:
#endif
	 mkint(n1.i, &arg0);
	 break;

      case T_REAL:
         mkreal(n1.r, &arg0);
         break;

      default:
	 fail();
      }
   }

struct b_iproc Bnumeric = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xnumeric),
   1,
   -1,
   0,
   0,
   {7, "numeric"}
   };
