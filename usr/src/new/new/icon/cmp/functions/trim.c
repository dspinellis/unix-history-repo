#include "../h/rt.h"

/*
 * trim(s1,c) - trim trailing characters in c from s1.
 */

Xtrim(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register i;
   char *sloc;
   char sbuf[MAXSTRING];
   int *cs, csbuf[CSETSIZE];
   static int spcset[CSETSIZE] = 
      cset_display(0, 0, 01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);
   defcset(&arg2, &cs, csbuf, spcset);

   arg0 = arg1;
   sloc = STRLOC(arg1) + STRLEN(arg1) - 1;
   while (sloc >= STRLOC(arg1) && tstb(*sloc, cs)) {
      sloc--;
      STRLEN(arg0)--;
      }
   }

struct b_iproc Btrim = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xtrim),
   2,
   -1,
   0,
   0,
   {4, "trim"}
   };
