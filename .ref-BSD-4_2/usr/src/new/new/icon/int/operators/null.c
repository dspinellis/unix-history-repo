#include "../h/rt.h"

/*
 * /x - test x for null value.
 */

null(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i, j;

   SetBound;
   arg0 = arg1;
   deref(&arg1);

   if (!NULLDESC(arg1))
      fail();
   ClearBound;
   }
struct b_iproc Bnull = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(null),
   1,
   -1,
   0,
   0,
   {1, "/"}
   };
