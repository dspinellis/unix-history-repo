#include "../h/rt.h"

/*
 * x >= y - test if x is numerically greater or equal to y.
 */

numge(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;
   if (numcmp(&arg1, &arg2, &arg0) < 0)
      fail();
   ClearBound;
   }

Opblock(numge,2,">=")
