#include "../h/rt.h"

/*
 * reverse(s) - reverse string s.
 */

Xreverse(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register char c, *floc, *lloc;
   register int slen;
   char sbuf[MAXSTRING];
   extern char *alcstr();

   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);

   slen = STRLEN(arg1);
   sneed(slen);
   STRLEN(arg0) = slen;
   STRLOC(arg0) = alcstr(STRLOC(arg1), slen);

   floc = STRLOC(arg0);
   lloc = floc + --slen;
   while (floc < lloc) {
      c = *floc;
      *floc++ = *lloc;
      *lloc-- = c;
      }
   }

struct b_iproc Breverse = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xreverse),
   1,
   -1,
   0,
   0,
   {7, "reverse"}
   };
