#include "../h/rt.h"

/*
 * any(c,s,i,j) - test if first character of s[i:j] is in c.
 */

Xany(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int i, j;
   long l1, l2;
   int *cs, csbuf[CSETSIZE];
   char sbuf[MAXSTRING];

   if (cvcset(&arg1, &cs, csbuf) == NULL)
      runerr(104, &arg1);
   if (defstr(&arg2, sbuf, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i > j)
      i = j;

   if (!tstb(STRLOC(arg2)[i-1], cs))
      fail();

   arg0.type = D_INTEGER;
   INTVAL(arg0) = i + 1;
   }

struct b_iproc Bany = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xany),
   4,
   -1,
   0,
   0,
   {3, "any"}
   };
