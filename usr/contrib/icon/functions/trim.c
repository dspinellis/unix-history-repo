#include "../h/rt.h"

/*
 * trim(s,c) - trim trailing characters in c from s.
 */

Xtrim(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   char *sloc;
   char sbuf[MAXSTRING];
   int *cs, csbuf[CSETSIZE];
   static int spcset[CSETSIZE] = /* just a blank */
      cset_display(0, 0, 01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

   /*
    * s must be a string; c defaults to a cset containing a blank.
    */
   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);
   defcset(&arg2, &cs, csbuf, spcset);

   /*
    * Start at the end of s and then back up until a character that is
    *  not in c is found.  The actual trimming is done by having a descriptor
    *  that points at the string of s, but has a reduced length.
    */
   arg0 = arg1;
   sloc = STRLOC(arg1) + STRLEN(arg1) - 1;
   while (sloc >= STRLOC(arg1) && tstb(*sloc, cs)) {
      sloc--;
      STRLEN(arg0)--;
      }
   }

Procblock(trim,2)
