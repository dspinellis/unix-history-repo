#include "../h/rt.h"

/*
 * bscan - set &subject and &pos upon entry to a scanning expression.
 *
 *  Arguments are:
 *      arg1 - new value for &subject
 *      arg2 - saved value of &subject
 *      arg3 - saved value of &pos
 */

bscan(nargs, arg3, arg2, arg1)
int nargs;
struct descrip arg3, arg2, arg1;
   {
   DclSave
   char sbuf[MAXSTRING];
   extern char *alcstr();
   struct descrip tsubject;

   SetBound;
   /*
    * Make a copy of the value for &subject and convert it to a string.
    */
   tsubject = arg1;
   switch (cvstr(&tsubject, sbuf)) {
      case NULL:
         runerr(103, &tsubject);
      case 1:
         /*
          * The new value for &subject wasn't a string.  Allocate the
          *  new value and fall through.
          */
         sneed(STRLEN(tsubject));
         STRLOC(tsubject) = alcstr(STRLOC(tsubject), STRLEN(tsubject));
      case 2:
         /*
          * Establish a new &subject value and set &pos to 1.
          */
         k_subject = tsubject;
         k_pos = 1;
      }
   suspend();

   /*
    * bscan has been resumed. Restore the old &subject and &pos values
    *  and fail.
    */
   k_subject = arg2;
   k_pos = INTVAL(arg3);
   fail();
   }
