#include "../h/rt.h"

/*
 * bal(c1,c2,c3,s,i,j) - find end of a balanced substring of s[i:j].
 *  Generates successive positions.
 */

Xbal(nargs, arg6, arg5, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg6, arg5, arg4, arg3, arg2, arg1, arg0;
   {
   register int i, j, cnt;
   register c;
   int t;
   long l1, l2;
   int *cs1, *cs2, *cs3;
   int csbuf1[CSETSIZE], csbuf2[CSETSIZE], csbuf3[CSETSIZE];
   char sbuf[MAXSTRING];
   static int lpar[CSETSIZE] =	/* '(' */
      cset_display(0, 0, 0400, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
   static int rpar[CSETSIZE] =	/* ')' */
      cset_display(0, 0, 01000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

   /*
    *  c1 defaults to &cset; c2 defaults to '(' (lpar); c3 defaults to
    *   ')' (rpar); s to &subject; i to &pos if s defaulted, 1 otherwise;
    *   j defaults to 0.
    */
   defcset(&arg1, &cs1, csbuf1, k_cset.bits);
   defcset(&arg2, &cs2, csbuf2, lpar);
   defcset(&arg3, &cs3, csbuf3, rpar);
   if (defstr(&arg4, sbuf, &k_subject))
      defint(&arg5, &l1, k_pos);
   else
      defint(&arg5, &l1, 1);
   defint(&arg6, &l2, 0);

   /*
    * Convert i and j to positions in s and order them.
    */
   i = cvpos(l1, STRLEN(arg4));
   j = cvpos(l2, STRLEN(arg4));
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   /*
    * Loop through characters in s[i:j].  When a character in cs2 is
    *  found, increment cnt; when a chracter in cs3 is found, decrement
    *  cnt.  When cnt is 0 there have been an equal number of occurrences
    *  of characters in cs2 and cs3, i.e., the string to the left of
    *  i is balanced.  If the string is balanced and the current character
    *  (s[i]) is in c1, suspend i.  Note that if cnt drops below zero,
    *  bal fails.
    */
   cnt = 0;
   while (i < j) {
      c = STRLOC(arg4)[i-1];
      if (cnt == 0 && tstb(c, cs1)) {
         arg0.type = D_INTEGER;
         INTVAL(arg0) = i;
         suspend();
         }
      if (tstb(c, cs2))
         cnt++;
      else if (tstb(c, cs3))
         cnt--;
      if (cnt < 0)
         fail();
      i++;
      }
   /*
    * Eventually fail.
    */
   fail();
   }

Procblock(bal,6)
