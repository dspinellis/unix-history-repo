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
   DeRef(arg1)
   DeRef(arg2)

   /*
    * Let equiv do all the work, failing if equiv indicates non-equivalence.
    */
   if (!equiv(&arg1, &arg2))
      fail();

   arg0 = arg2;
   ClearBound;
   }

Opblock(eqv,2,"===")
