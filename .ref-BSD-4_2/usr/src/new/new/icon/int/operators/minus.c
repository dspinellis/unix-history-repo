#include "../h/rt.h"

/*
 * x - y - subtract y from x.
 */

minus(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int t1, t2;
   union numeric n1, n2;
   extern long cksub();

   SetBound;
   if ((t1 = cvnum(&arg1, &n1)) == NULL)
      runerr(102, &arg1);
   if ((t2 = cvnum(&arg2, &n2)) == NULL)
      runerr(102, &arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT)
      mkint(cksub(n1.i, n2.i), &arg0);
   else {
      if (t1 == T_LONGINT)
         n1.r = n1.i;
      if (t2 == T_LONGINT)
         n2.r = n2.i;
      mkreal(n1.r - n2.r, &arg0);
      }
   ClearBound;
   }
struct b_iproc Bminus = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(minus),
   2,
   -1,
   0,
   0,
   {1, "-"}
   };
