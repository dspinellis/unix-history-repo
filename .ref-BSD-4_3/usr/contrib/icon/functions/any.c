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

   /*
    * c must be a cset.  s defaults to &subject; i defaults to &pos if s
    *  defaulted, 1 otherwise.  j defaults to 0.
    */
   if (cvcset(&arg1, &cs, csbuf) == NULL)
      runerr(104, &arg1);
   if (defstr(&arg2, sbuf, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   /*
    * Convert i and j to positions in s. If i == j then the specified
    *  substring of s is empty and any fails. Otherwise make i the smaller of
    *  the two.  (j is of no further use.)
    */
   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i == j)
      fail();
   if (i > j)
      i = j;

   /*
    * If s[i] is not in the cset c, fail.
    */
   if (!tstb(STRLOC(arg2)[i-1], cs))
      fail();

   /*
    * Return pos(s[i+1]).
    */
   arg0.type = D_INTEGER;
   INTVAL(arg0) = i + 1;
   }

Procblock(any,4)
