#include "../h/rt.h"

/*
 * e1 to e2 by e3 - generate successive values.
 */

toby(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   DclSave
   long from, to, by;

   SetBound;
   /*
    * e1 (from), e2 (to), and e3 (by) must be integers.  Also, e3 must
    *  not be zero.
    */
   if (cvint(&arg1, &from) == NULL)
      runerr(101, &arg1);
   if (cvint(&arg2, &to) == NULL)
      runerr(101, &arg2);
   if (cvint(&arg3, &by) == NULL)
      runerr(101, &arg3);
   if (by == 0)
      runerr(211, &arg3);

   /*
    * Count up or down (depending on relationship of from and to) and
    *  suspend each value in sequence, failing when the limit has been
    *  exceeded.
    */
   while ((from <= to && by > 0) || (from >= to && by < 0)) {
      mkint(from, &arg0);
      suspend();
      from += by;
      }
   fail();
   }

Opblock(toby,3,"toby")
