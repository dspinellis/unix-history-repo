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
   if ((t1 = cvnum(&arg1, &n1)) == NULL)
      runerr(102, &arg1);
   if ((t2 = cvnum(&arg2, &n2)) == NULL)
      runerr(102, &arg2);

   if (t1 == T_LONGINT && t2 == T_LONGINT)
      mkint(ipow(n1.i, n2.i), &arg0);
   else {
      if (t1 == T_LONGINT)
         n1.r = n1.i;
      if (t2 == T_LONGINT)
         n2.r = n2.i;
      if (n1.r == 0.0 && n2.r <= 0.0)
	 runerr(204, NULL);
      if (n1.r < 0.0 && t2 == T_REAL)
         runerr(206, NULL);
      mkreal(pow(n1.r,n2.r), &arg0);
      }
   ClearBound;
   }

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
struct b_iproc Bpower = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(power),
   2,
   -1,
   0,
   0,
   {1, "^"}
   };
