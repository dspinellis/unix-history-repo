#include "../h/rt.h"

/*
 * e1 to e2 by e3 - generate successive values.
 * Generator.
 */

toby(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   DclSave
   long from, to, by;

   SetBound;
   if (cvint(&arg1, &from) == NULL)
      runerr(101, &arg1);
   if (cvint(&arg2, &to) == NULL)
      runerr(101, &arg2);
   if (cvint(&arg3, &by) == NULL)
      runerr(101, &arg3);
   if (by == 0)
      runerr(211, &arg3);

   while ((from <= to && by > 0) || (from >= to && by < 0)) {
      mkint(from, &arg0);
      suspend();
      from += by;
      }
   fail();
   }
struct b_iproc Btoby = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(toby),
   3,
   -1,
   0,
   0,
   {4, "toby"}
   };
