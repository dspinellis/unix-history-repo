#include "../h/rt.h"

/*
 * bal(c1,c2,c3,s,i,j) - match a balanced substring of s[i:j].
 * Generator.
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
   static int lpar[CSETSIZE] =
      cset_display(0, 0, 0400, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
   static int rpar[CSETSIZE] =
      cset_display(0, 0, 01000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

   defcset(&arg1, &cs1, csbuf1, k_cset.bits);
   defcset(&arg2, &cs2, csbuf2, lpar);
   defcset(&arg3, &cs3, csbuf3, rpar);
   if (defstr(&arg4, sbuf, &k_subject))
      defint(&arg5, &l1, k_pos);
   else
      defint(&arg5, &l1, 1);
   defint(&arg6, &l2, 0);

   i = cvpos(l1, STRLEN(arg4));
   j = cvpos(l2, STRLEN(arg4));
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   cnt = 0;
   while (i < j) {	        /* suspend for each occurrence */
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
   fail();
   }

struct b_iproc Bbal = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xbal),
   6,
   -1,
   0,
   0,
   {3, "bal"}
   };
