#include "../h/rt.h"

/*
 * .x - produce value of x by dereferencing it.
 */

value(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   SetBound;
   arg0 = arg1;
   DeRef(arg0)
   ClearBound;
   }

Opblock(value,1,".")
