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

   /*
    * Get length and starting address of both strings.
    */
   l1 = STRLEN(*d1);
   s1 = STRLOC(*d1);
   l2 = STRLEN(*d2);
   s2 = STRLOC(*d2);

   /*
    * Set minlen to length of the shorter string.
    */
   minlen = (l1 <= l2) ? l1 : l2;

   /*
    * Compare as many bytes as are in the smaller string.  If an
    *  inequality is found, return the difference of the differing
    *  bytes.
    */
   while (minlen--)
      if (*s1++ != *s2++)
         return ((*--s1 & 0377) - (*--s2 & 0377));

   /*
    * The strings compared equal for the length of the shorter.  Return
    *  the difference in their lengths.  (Thus, the strings must be of
    *  the same length to be equal.)
    */
   return (l1 - l2);
   }
