#include "../h/rt.h"

/*
 * x ^ y - raise x to the y power.
 */

power(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int t1, t2;
   union numeric n1, n2;
   extern double pow();
   extern long ipow();

   SetBound;
   /*
    * x and y must be numeric.  Save the cvnum return values for later use.
    */
   if ((t1 = cvnum(&arg1, &n1)) == NULL)
      runerr(102, &arg1);
   if ((t2 = cvnum(&arg2, &n2)) == NULL)
      runerr(102, &arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT)
      /*
       * Both x and y are integers.  Perform integer exponentiation
       *  and place the result in arg0 as the return value.
       */
      mkint(ipow(n1.integer, n2.integer), &arg0);
   else {
      /*
       * Either x or y is real, convert the other to a real, perform
       *  real exponentiation and place the result in arg0 as the
       *  return value.
       */
      if (t1 == T_LONGINT)
         n1.real = n1.integer;
      if (t2 == T_LONGINT)
         n2.real = n2.integer;
      if (n1.real == 0.0 && n2.real <= 0.0)
         /*
          * Tried to raise zero to a negative power.
          */
         runerr(204, NULL);
      if (n1.real < 0.0 && t2 == T_REAL)
         /*
          * Tried to raise a negative number to a real power.
          */
         runerr(206, NULL);
      mkreal(pow(n1.real,n2.real), &arg0);
      }
   ClearBound;
   }

Opblock(power,2,"^")

long ipow(n1, n2)
long n1, n2;
   {
   long result;

   if (n1 == 0 && n2 <= 0)
      runerr(204, NULL);
   if (n2 < 0)
      return (0.0);
   result = 1L;
   while (n2 > 0) {
      if (n2 & 01L)
         result *= n1;
      n1 *= n1;
      n2 >>= 1;
      }
   return (result);
   }

