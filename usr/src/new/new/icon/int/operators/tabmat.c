#include "../h/rt.h"

/*
 * =x - tab(match(x)).
 * Reversible.
 */

tabmat(nargs, oldsubj, arg1, arg0)
int nargs;
struct descrip oldsubj, arg1, arg0;
   {
   DclSave
   register int l;
   register char *s1, *s2;
   int i, j;
   char sbuf[MAXSTRING];

   SetBound;
   if (cvstr(&arg1,sbuf) == NULL)
      runerr(103, &arg1);

   oldsubj = k_subject;
   i = k_pos;
   j = STRLEN(k_subject) - i + 1;

   if (j < STRLEN(arg1))
      fail();

   s1 = STRLOC(arg1);
   s2 = STRLOC(k_subject) + i - 1;
   l = STRLEN(arg1);
   while (l-- > 0) {
      if (*s1++ != *s2++)
	 fail();
      }

   l = STRLEN(arg1);
   k_pos += l;
   arg0 = arg1;
   suspend();

   k_subject = oldsubj;
   k_pos = i;
   fail();
   }
struct b_iproc Btabmat = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(tabmat),
   2,
   -1,
   -1,
   0,
   {1, "="}
   };
