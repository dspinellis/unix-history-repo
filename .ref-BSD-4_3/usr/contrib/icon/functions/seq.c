#include "../h/rt.h"
#ifdef XPX
/*
 * seq(e1,e2)  - generate e1, e1+e2, e1+e2+e2, ... .
 */

Xseq(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   long from, by;

   /*
    * Default e1 and e2 to 1.
    */
   defint(&arg1, &from, 1);
   defint(&arg2, &by, 1);
   
   /*
    * Produce error if e2 is 0, i.e., infinite sequence of e1's.
    */
   if (by == 0)
      runerr(211, &arg2);

   /*
    * Suspend sequence, stopping when largest or smallest integer
    *  is reached.
    */
   while ((from <= MAXLONG && by > 0) || (from >= MINLONG && by < 0)) {
      mkint(from, &arg0);
      suspend();
      from += by;
      }
   fail();
   }

Procblock(seq,2)

#else XPX
char junk;			/* prevent null object file */
#endif XPX
