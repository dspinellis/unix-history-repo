#include "../h/rt.h"

/*
 * repl(s,n) - concatenate n copies of string s.
 */

Xrepl(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int cnt, i;
   register char *sloc;
   long l1;
   char sbuf[MAXSTRING];
   extern char *alcstr();

   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);
   switch (cvint(&arg2, &l1)) {
      case T_INTEGER:   if ((cnt = (int)l1) >= 0) break;
#ifndef BIT32
      case T_LONGINT:   runerr(205, &arg2);
#endif
      default:          runerr(101, &arg2);
      }

   if ((l1 * STRLEN(arg1)) > MAXSHORT)
      runerr(302, NULL);
   if (cnt == 0) {
      arg0.type = D_NULL;
      INTVAL(arg0) = 1;
      }
   else {
      sneed(cnt * STRLEN(arg1));
      sloc = alcstr(STRLOC(arg1), STRLEN(arg1));
      cnt--;
      while (cnt--)
         alcstr(STRLOC(arg1), STRLEN(arg1));
      STRLEN(arg0) = (int)l1 * STRLEN(arg1);
      STRLOC(arg0) = sloc;
      }
   }

struct b_iproc Brepl = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xrepl),
   2,
   -1,
   0,
   0,
   {4, "repl"}
   };
