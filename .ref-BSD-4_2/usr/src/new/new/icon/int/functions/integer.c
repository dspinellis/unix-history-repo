#include "../h/rt.h"

/*
 * integer(x) - convert x to integer.
 */

Xinteger(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   long l;

   switch (cvint(&arg1, &l)) {
      case T_INTEGER:
#ifndef BIT32
      case T_LONGINT:
#endif
	 mkint(l, &arg0);
	 break;
      default:
	 fail();
      }
   }

struct b_iproc Binteger = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xinteger),
   1,
   -1,
   0,
   0,
   {7, "integer"}
   };
