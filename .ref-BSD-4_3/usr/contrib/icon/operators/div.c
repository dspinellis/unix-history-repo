#include "../h/rt.h"

/*
 * x / y - divide y into x.
 */

div(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int t1, t2;
   union numeric n1, n2;

   SetBound;
   /*
    * x and y must be numbers.
    */
   if ((t1 = cvnum(&arg1, &n1)) == NULL)
      runerr(102, &arg1);
   if ((t2 = cvnum(&arg2, &n2)) == NULL)
      runerr(102, &arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT) {
      /*
       * x and y are both integers, just divide them and return the result.
       */
      if (n2.integer == 0L)
         runerr(201, &arg2);
      mkint(n1.integer / n2.integer, &arg0);
      }
   else {
      /*
       * Either x or y or both is real, convert the real values to integers,
       *  divide them, and return the result.
       */
      if (t1 == T_LONGINT)
         n1.real = n1.integer;
      if (t2 == T_LONGINT)
         n2.real = n2.integer;
      mkreal(n1.real / n2.real, &arg0);
      }
   ClearBound;
   }

Opblock(div,2,"/")
