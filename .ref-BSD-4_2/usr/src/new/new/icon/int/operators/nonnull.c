#include "../h/rt.h"

/*
 * \x - test x for non-null value.
 */

nonnull(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i, j;

   SetBound;
   arg0 = arg1;
   deref(&arg1);

   if (NULLDESC(arg1))
      fail();
   ClearBound;
   }
struct b_iproc Bnonnull = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(nonnull),
   1,
   -1,
   0,
   0,
   {1, "\\"}
   };
