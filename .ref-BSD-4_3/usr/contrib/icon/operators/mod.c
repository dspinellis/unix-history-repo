#include "../h/rt.h"

/*
 * x % y - take remainder of x / y.
 */

mod(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int t1, t2;
   union numeric n1, n2;

   SetBound;
   /*
    * x and y must be numeric.  Save the cvnum return values for later use.
    */
   if ((t1 = cvnum(&arg1, &n1)) == NULL)
      runerr(102, &arg1);
   if ((t2 = cvnum(&arg2, &n2)) == NULL)
      runerr(102, &arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT) {
      /*
       * Both x and y are integers.  If y is 0, generate an error because
       *  it's divide by 0.  Otherwise, just return the modulus of the
       *  two arguments.
       */
      if (n2.integer == 0L)
         runerr(202, &arg2);
      mkint(n1.integer % n2.integer, &arg0);
      }
   else {
      /*
       * Either x or y is real, convert the other to a real, perform
       *  the modulation, convert the result to an integer and place it
       *  in arg0 as the return value.
       */
      if (t1 == T_LONGINT)
         n1.real = n1.integer;
      if (t2 == T_LONGINT)
         n2.real = n2.integer;
      mkreal(n1.real - n2.real * (int)(n1.real / n2.real), &arg0);
      }
   ClearBound;
   }

Opblock(mod,2,"%")
