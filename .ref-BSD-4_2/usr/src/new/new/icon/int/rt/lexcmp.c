#include "../h/rt.h"

/*
 * lexcmp - lexically compare two strings.
 */

lexcmp(d1, d2)
struct descrip *d1, *d2;
   {
   register char *s1, *s2;
   register int minlen;
   int l1, l2;

   l1 = STRLEN(*d1);
   s1 = STRLOC(*d1);
   l2 = STRLEN(*d2);
   s2 = STRLOC(*d2);

   minlen = (l1 <= l2) ? l1 : l2;

   while (minlen--)
      if (*s1++ != *s2++)
         return ((*--s1 & 0377) - (*--s2 & 0377));

   return (l1 - l2);
   }
