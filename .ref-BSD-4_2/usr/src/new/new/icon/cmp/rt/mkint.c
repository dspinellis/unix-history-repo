#include "../h/rt.h"

/*
 * mkint(l,d) - make a descriptor for l, either a long or short integer.
 * Unstable!
 */

mkint(l, d)
long l;
register struct descrip *d;
   {
#ifndef VAX
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
#else
   d->type = D_INTEGER;
   INTVAL(*d) = (int)l;
#endif
   }
