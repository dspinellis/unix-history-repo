#include "../h/rt.h"

/*
 * system(s) - execute string s as a system command.
 */

Xsystem(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   char sbuf[MAXSTRING];

   deref(&arg1);

   if (!QUAL(arg1) || STRLEN(arg1) < 0)
      runerr(103, &arg1);
   if (STRLEN(arg1) >= MAXSTRING)
      runerr(210, &arg1);
   qtos(&arg1, sbuf);

   mkint((long)((system(sbuf) >> 8) & 0377), &arg0);
   }

struct b_iproc Bsystem = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xsystem),
   1,
   -1,
   0,
   0,
   {6, "system"}
   };
