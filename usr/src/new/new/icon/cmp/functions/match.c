#include "../h/rt.h"

/*
 * match(s1,s2,i,j) - test if s1 is prefix of s2[i:j].
 */

Xmatch(nargs, arg4, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg4, arg3, arg2, arg1, arg0;
   {
   register int i;
   register char *s1, *s2;
   int j, t;
   long l1, l2;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];

   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if (defstr(&arg2, sbuf2, &k_subject))
      defint(&arg3, &l1, k_pos);
   else
      defint(&arg3, &l1, 1);
   defint(&arg4, &l2, 0);

   i = cvpos(l1, STRLEN(arg2));
   j = cvpos(l2, STRLEN(arg2));

   if (i > j) {                 /* convert to substring */
      t = i;
      i = j;
      j = t - j;
      }
   else
      j = j - i;

   if (j < STRLEN(arg1))
      fail();

   s1 = STRLOC(arg1);
   s2 = STRLOC(arg2) + i - 1;
   for (j = STRLEN(arg1); j > 0; j--)
      if (*s1++ != *s2++)
         fail();

   arg0.type = D_INTEGER;
   INTVAL(arg0) = i + STRLEN(arg1);
   }

struct b_iproc Bmatch = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xmatch),
   4,
   -1,
   0,
   0,
   {5, "match"}
   };
