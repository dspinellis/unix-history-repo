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

   /*
    * Make sure that s is a string.
    */
   if (cvstr(&arg1, sbuf) == NULL)
      runerr(103, &arg1);

   /*
    * Ensure that there is enough room and allocate a copy of s.
    */
   slen = STRLEN(arg1);
   sneed(slen);
   STRLEN(arg0) = slen;
   STRLOC(arg0) = alcstr(STRLOC(arg1), slen);

   /*
    * Point floc at the start of s and lloc at the end of s.  Work floc
    *  and sloc along s in opposite directions, swapping the characters
    *  at floc and lloc.
    */
   floc = STRLOC(arg0);
   lloc = floc + --slen;
   while (floc < lloc) {
      c = *floc;
      *floc++ = *lloc;
      *lloc-- = c;
      }
   }

Procblock(reverse,1)
