#include "../h/rt.h"

/*
 * mkint - make an integer descriptor for l in *d.  A long integer is used
 *  if the value is too large for a regular integer.
 */

mkint(l, d)
long l;
register struct descrip *d;
   {
#ifdef LONGS
   extern struct b_int *alclint();

   if (l < (long)(int)MINSHORT || l > (long)(int)MAXSHORT) {
      hneed(sizeof(struct b_int));
      d->type = D_LONGINT;
      BLKLOC(*d) = alclint(l);
      }
   else {
      d->type = D_INTEGER;
      INTVAL(*d) = (int)l;
      }
#else LONGS
   d->type = D_INTEGER;
   INTVAL(*d) = (int)l;
#endif LONGS
   }
