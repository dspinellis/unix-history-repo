#include "../h/rt.h"

/*
 * s1 <<= s2 - test if s1 is lexically less than or equal to s2.
 */

lexle(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   register int t;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern char *alcstr();

   SetBound;
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   if ((t = cvstr(&arg2, sbuf2)) == NULL)
      runerr(103, &arg2);

   if (lexcmp(&arg1, &arg2) > 0)
      fail();

   arg0 = arg2;
   if (t == 1)		/* string needs to be allocated */
      STRLOC(arg0) = alcstr(STRLOC(arg0), STRLEN(arg0));
   ClearBound;
   }
struct b_iproc Blexle = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(lexle),
   2,
   -1,
   0,
   0,
   {3, "<<="}
   };
