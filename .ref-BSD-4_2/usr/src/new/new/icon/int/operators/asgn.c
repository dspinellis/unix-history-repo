#include "../h/rt.h"

/*
 * x := y - assign y to x.
 */

asgn(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;
   if (QUAL(arg1) || !VAR(arg1))
      runerr(111, &arg1);
   arg0 = arg1;
   deref(&arg2);
   doasgn(&arg1, &arg2);
   ClearBound;
   }
struct b_iproc Basgn = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(asgn),
   2,
   -1,
   0,
   0,
   {2, ":="}
   };
