#include "../h/rt.h"

/*
 * upto(c,s,i,j) - find each occurrence in s[i:j] of a character in c.
 * Generates successive positions.
 */

Xupto(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int i, j;
   int t;
   long l1, l2;
   int *cs, csbuf[CSETSIZE];
   char sbuf[MAXSTRING];

   /*
    * c must be a cset.  s defaults to &subject; i defaults to &pos if
    *  s defaulted, 1 otherwise; j defaults to 0.
    */
   if (cvcset(&arg1, &cs, csbuf) == NULL)
      runerr(104, &arg1);
   if (defstr(&arg2, sbuf, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   /*
    * Convert i and j to positions in s and order them.
    */
   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   /*
    * Look through s[i:j] and suspend position of each occurrence of
    *  of a character in c.
    */
   while (i < j) {
      if (tstb(STRLOC(arg2)[i-1], cs)) {
         arg0.type = D_INTEGER;
         INTVAL(arg0) = i;
         suspend();
         }
      i++;
      }
   /*
    * Eventually fail.
    */
   fail();
   }

Procblock(upto,4)
