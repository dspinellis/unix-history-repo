#include "../h/rt.h"

/*
 * .x - dereference x.
 */

value(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   SetBound;
   arg0 = arg1;
   deref(&arg0);
   ClearBound;
   }
struct b_iproc Bvalue = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(value),
   1,
   -1,
   0,
   0,
   {1, "."}
   };
