#include "../h/rt.h"

/*
 * tab(i) - set &pos to i, return substring of &subject spanned.
 *  Reverses effects if resumed..
 */

Xtab(nargs, oldsubj, arg1, arg0)
int nargs;
struct descrip oldsubj, arg1, arg0;
   {
   register int i, j;
   int t, oldpos;
   long l1;

   /*
    * i must be an integer.
    */
   if (cvint(&arg1,&l1) == NULL)
      runerr(101, &arg1);

   /*
    * Convert j to an absolute position.
    */
   j = cvpos(l1, STRLEN(k_subject));

   /*
    * Save old &subject and &pos.  Local variable i holds &pos
    *  before the tab.
    */
   oldsubj = k_subject;
   oldpos = i = k_pos;

   /*
    * Set new &pos.
    */
   k_pos = j;

   /*
    *  Make j the length of the substring &subject[i:j]
    */
   if (i > j) {
      t = i;
      i = j;
      j = t - j;
      }
   else
      j = j - i;

   /*
    * Suspend the portion of &subject that was tabbed over.
    */
   STRLOC(arg0) = STRLOC(k_subject) + i - 1;
   STRLEN(arg0) = j;
   suspend();

   /*
    * If tab is resumed, restore the old subject and position
    *  and fail.
    */
   k_subject = oldsubj;
   k_pos = oldpos;
   fail();
   }

Procblock(tab,2)
