#include "../h/rt.h"

/*
 * =x - tab(match(x)).
 * Reverses effects if resumed.
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
   /*
    * x must be a string.
    */
   if (cvstr(&arg1,sbuf) == NULL)
      runerr(103, &arg1);

   /*
    * Make a copy of &subject and &pos.
    */
   oldsubj = k_subject;
   i = k_pos;
   
   /*
    * Fail if &subject[&pos:0] is not of sufficient length to contain x.
    */
   j = STRLEN(k_subject) - i + 1;
   if (j < STRLEN(arg1))
      fail();

   /*
    * Get pointers to x (s1) and &subject (s2).  Compare them on a bytewise
    *  basis and fail if s1 doesn't match s2 for *s1 characters.
    */
   s1 = STRLOC(arg1);
   s2 = STRLOC(k_subject) + i - 1;
   l = STRLEN(arg1);
   while (l-- > 0) {
      if (*s1++ != *s2++)
         fail();
      }

   /*
    * Increment &pos to tab over the matched string and suspend the
    *  matched string.
    */
   l = STRLEN(arg1);
   k_pos += l;
   arg0 = arg1;
   suspend();

   /*
    * tabmat has been resumed, restore &subject and &pos and fail.
    */
   k_subject = oldsubj;
   k_pos = i;
   fail();
   }

Opblockx(tabmat,2,"=",1)
