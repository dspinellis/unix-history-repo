#include "../h/rt.h"

/*
 * x ~=== y - test inequivalence of x and y.
 */

neqv(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;

   /*
    * equiv does all the work.
    */
   DeRef(arg1)
   DeRef(arg2)
   if (equiv(&arg1, &arg2))
      fail();
   arg0 = arg2;
   ClearBound;
   }

Opblock(neqv,2,"~===")
