#include "../h/rt.h"

/*
 * real(x) - convert x to real.
 */

Xreal(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   double r;

   deref(&arg1);
   if (!QUAL(arg1) && TYPE(arg1) == T_REAL)
      arg0 = arg1;
   else if (cvreal(&arg1, &r) == T_REAL)
      mkreal(r, &arg0);
   else
      fail();
   }

struct b_iproc Breal = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xreal),
   1,
   -1,
   0,
   0,
   {4, "real"}
   };
