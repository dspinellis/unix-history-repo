#include "../h/rt.h"

/*
 * \x - test x for non-null value.
 */

nonnull(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {

   DclSave
   SetBound;

   /*
    * If x is not null, it is returned, otherwise, the function fails.
    *  Because the pre-dereference value of x is the return value (if
    *  any), x is copied into arg0.
    */
   arg0 = arg1;
   DeRef(arg1)
   if (NULLDESC(arg1))
      fail();
   ClearBound;
   }

Opblock(nonnull,1,"\\")
