#include "../h/rt.h"

/*
 * numcmp - compare two numbers
 */

numcmp(arg1, arg2, arg0)
struct descrip *arg1, *arg2, *arg0;
   {
   register int result;
   union numeric n1, n2;
   int t1, t2;
   long l;
   double fresult;
   extern long cksub();

   if ((t1 = cvnum(arg1, &n1)) == NULL)
      runerr(102, arg1);
   if ((t2 = cvnum(arg2, &n2)) == NULL)
      runerr(102, arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT) {
      l = cksub(n1.i, n2.i);
      if (l < 0L)
         result = -1;
      else if (l > 0L)
         result = 1;
      else
         result = 0;
      mkint(n2.i, arg0);
      }
   else {
      if (t1 == T_LONGINT)
         n1.r = n1.i;
      if (t2 == T_LONGINT)
         n2.r = n2.i;
      fresult = n1.r - n2.r;
      if (fresult < 0.0)
         result = -1;
      else if (fresult > 0.0)
         result = 1;
      else
         result = 0;
      mkreal(n2.r, arg0);
      }

   return (result);             /* return result in r0 */
   }
