#include "../h/rt.h"

/*
 * x === y - test equivalence of x and y.
 */

eqv(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;
   deref(&arg1);
   deref(&arg2);

   if (!equiv(&arg1, &arg2))
      fail();
   arg0 = arg2;
   ClearBound;
   }
struct b_iproc Beqv = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(eqv),
   2,
   -1,
   0,
   0,
   {3, "==="}
   };
