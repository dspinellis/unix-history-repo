#include "../h/rt.h"

/*
 * tab(i) - set &pos to i, return substring of &subject spanned.
 * Reversible.
 */

Xtab(nargs, oldsubj, arg1, arg0)
int nargs;
struct descrip oldsubj, arg1, arg0;
   {
   register int i, j;
   int t, oldpos;
   long l1;

   if (cvint(&arg1,&l1) == NULL)
      runerr(101, &arg1);

   j = cvpos(l1, STRLEN(k_subject));

   oldsubj = k_subject;		/* save old &subject and &pos */
   oldpos = i = k_pos;

   k_pos = j;			/* set new &pos */

   if (i > j) {			/* convert section to substring */
      t = i;
      i = j;
      j = t - j;
      }
   else
      j = j - i;

   STRLOC(arg0) = STRLOC(k_subject) + i - 1;
   STRLEN(arg0) = j;
   suspend();

   k_subject = oldsubj;
   k_pos = oldpos;
   fail();
   }

struct b_iproc Btab = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xtab),
   2,
   -1,
   0,
   0,
   {3, "tab"}
   };
