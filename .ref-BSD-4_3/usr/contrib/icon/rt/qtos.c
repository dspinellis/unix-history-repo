#include "../h/rt.h"

/*
 * qtos - convert a qualified string named by *d to a C-style string in
 *  in str.  At most MAXSTRING characters are copied into str.
 */

qtos(d, str)
struct descrip *d;
char *str;
   {
   register int cnt, slen;
   register char *c;

   c = STRLOC(*d);
   slen = STRLEN(*d);
   for (cnt = MIN(slen, MAXSTRING - 1); cnt > 0; cnt--)
      *str++ = *c++;
   *str = '\0';
   }
