#include "../h/rt.h"

/*
 * numcmp - compare two numbers.  Returns -1, 0, 1 for arg1 <, =, > arg2.
 *  arg0 is made into a descriptor for the return value.
 */

numcmp(arg1, arg2, arg0)
struct descrip *arg1, *arg2, *arg0;
   {
   register int result;
   union numeric n1, n2;
   int t1, t2;
   /*
    * Be sure that both arg1 and arg2 are numeric.
    */

   if ((t1 = cvnum(arg1, &n1)) == NULL)
      runerr(102, arg1);
   if ((t2 = cvnum(arg2, &n2)) == NULL)
      runerr(102, arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT) {
   /*
    *  arg1 and arg2 are both integers, compare them and
    *  create an integer descriptor in arg0
    */

          result = 0;
          if (n1.integer < n2.integer) result = -1;
          else if (n1.integer != n2.integer) result = 1;
      mkint(n2.integer, arg0);
      }
   else {

   /*
    *  Either arg1 or arg2 is real. Convert the other to a real,
    *  compare them and create a real descriptor in arg0.
    */

      if (t1 == T_LONGINT)
         n1.real = n1.integer;
      if (t2 == T_LONGINT)
         n2.real = n2.integer;
          result = 0;
          if (n1.real < n2.real) result = -1;
          else if (n1.real != n2.real) result = 1;
      mkreal(n2.real, arg0);
      }

   return (result);             /* return result in r0 */
   }

