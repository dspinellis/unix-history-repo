#include "../h/rt.h"

/*
 * cvcset(d, cs, csbuf) - convert d to a cset and
 *  make cs point to it, using csbuf as a buffer if necessary.
 */

cvcset(d, cs, csbuf)
register struct descrip *d;
int **cs, *csbuf;
   {
   register char *s;
   register int l;
   char sbuf[MAXSTRING];

      DeRef(*d)

   if (!QUAL(*d) && TYPE(*d) == T_CSET) {
      *cs = BLKLOC(*d)->cset.bits;
      return (T_CSET);
      }

   if (cvstr(d, sbuf) == NULL)
      return (NULL);

   for (l = 0; l < CSETSIZE; l++)
      csbuf[l] = 0;

   s = STRLOC(*d);
   l = STRLEN(*d);
   while (l--) {
      setb(*s, csbuf);
      s++;
      }
   *cs = csbuf;
   return (1);
   }
