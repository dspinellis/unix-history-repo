#include "../h/rt.h"

/*
 * qtos - string qualifier to C string conversion.
 *   (truncates string to MAXSTRING long)
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
