#include "../h/rt.h"

/*
 * x == y - test if x is lexically equal to y.
 */

lexeq(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   register int t, i;
   register char *s1, *s2;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern char *alcstr();

   SetBound;
   /*
    * x and y must be strings.  Save the cvstr return value for y because
    *  y is the result (if any).
    */
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if ((t = cvstr(&arg2, sbuf2)) == NULL)
      runerr(103, &arg2);

   /*
    * If the strings have different lengths they can't be equal
    */
   if (STRLEN(arg1) != STRLEN(arg2))
      fail();

   /*
    * compare the strings
    */
   i = STRLEN(arg1);
   s1 = STRLOC(arg1);
   s2 = STRLOC(arg2);
   while (i--)
      if (*s1++ != *s2++)
         fail();

   /*
    * Return y as the result of the comparison.  If y was converted to
    *  a string, a copy of it is allocated.
    */
   arg0 = arg2;
   if (t == 1)
      STRLOC(arg0) = alcstr(STRLOC(arg0), STRLEN(arg0));
   ClearBound;
   }

Opblock(lexeq,2,"==")
