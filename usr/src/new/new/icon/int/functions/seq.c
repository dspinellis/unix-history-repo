#include "../h/rt.h"
#ifdef EXT
/*
 * seq(e1,e2) generate {e1, e1+e2, e1+e2+e2, ...}
 * Generator.
 */

seq(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   long from, by;

   defint(&arg1, &from, 1);
   defint(&arg2, &by, 1);
   
   if (by == 0)
      runerr(211, &arg2);

   while ((from <= MAXLONG && by > 0) || (from >= MINLONG && by < 0)) {
      mkint(from, &arg0);
      suspend();
      from += by;
      }
   fail();
   }
struct b_iproc Bseq = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(seq),
   2,
   -1,
   0,
   0,
   {3, "seq"}
   };
#endif EXT
