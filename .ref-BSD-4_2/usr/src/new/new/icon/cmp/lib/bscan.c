#include "../h/rt.h"

/*
 * bscan - set new &subject upon entry to a scan statement.
 * Bscan takes 3 arguments; arg1 is the new value for &subject.
 * Arg2 is the saved value of &subject, and arg3 is the saved
 * value of &pos.  &pos is also set to 1.
 * Bscan suspends once it has set the new &subject; on failure
 * the old &subject and &pos are restored, and the failure is
 * propagated.
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
   tsubject = arg1;
   switch (cvstr(&tsubject, sbuf)) {
      case NULL:
	 runerr(103, &tsubject);
      case 1:
	 sneed(STRLEN(tsubject));
         STRLOC(tsubject) = alcstr(STRLOC(tsubject), STRLEN(tsubject));
      case 2:
         k_subject = tsubject;
         k_pos = 1;
      }
   suspend();

   k_subject = arg2;
   k_pos = INTVAL(arg3);

   fail();
   }
