#include "../h/rt.h"

/*
 * x || y - concatenate strings x and y.
 */

cat(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern char *alcstr();

   SetBound;
   /*
    * x and y must be strings.
    */
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if (cvstr(&arg2, sbuf2) == NULL)
      runerr(103, &arg2);

   /*
    * Ensure space for the resulting string.
    */
   sneed(STRLEN(arg1)+STRLEN(arg2));
   if (STRLOC(arg1) + STRLEN(arg1) == sfree)
      /*
       * The end of x is at the end of the string space, hence, x was the
       *  last string allocated.  x is not recopied, rather, y is appended
       *  to the string space and the result is pointed at the start of x.
       */
      STRLOC(arg0) = STRLOC(arg1);
   else
      /*
       * Append x to the end of the string space and point the result
       *  at the start of x.
       */
      STRLOC(arg0) = alcstr(STRLOC(arg1),STRLEN(arg1));
   /*
    * Append y to the end of the string space.
    */
   alcstr(STRLOC(arg2),STRLEN(arg2));
   /*
    *  Set the length of the result and return.
    */
   STRLEN(arg0) = STRLEN(arg1) + STRLEN(arg2);
   ClearBound;
   }

Opblock(cat,2,"||")
