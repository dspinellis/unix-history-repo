#include "../h/rt.h"

/*
 * many(c,s,i,j) - find longest prefix of s[i:j] of characters in c.
 */

Xmany(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int i, j;
   int t, *cs, csbuf[CSETSIZE];
   long l1, l2;
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
   if (i == j)
      fail();
   if (i > j) {
      t = i;
      i = j;
      j = t;
      }

   if (!tstb(STRLOC(arg2)[i-1], cs))
      fail();

   i++;
   while (i < j && tstb(STRLOC(arg2)[i-1], cs))
      i++;

   arg0.type = D_INTEGER;
   INTVAL(arg0) = i;
   }

struct b_iproc Bmany = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xmany),
   4,
   -1,
   0,
   0,
   {4, "many"}
   };
