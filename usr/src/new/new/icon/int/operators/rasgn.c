#include "../h/rt.h"

/*
 * x <- y - assign y to x.
 * Reversible.
 */

rasgn(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   SetBound;
   if (QUAL(arg1) || !VAR(arg1))
      runerr(111, &arg1);
   arg0 = arg1;
   deref(&arg1);
   deref(&arg2);
   doasgn(&arg0, &arg2); 	/* do the assignment */
   suspend(); 			/* suspend */
   doasgn(&arg0, &arg1);     	/* reverse the assignment */
   fail();
   }
struct b_iproc Brasgn = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(rasgn),
   2,
   -1,
   0,
   0,
   {2, "<-"}
   };
