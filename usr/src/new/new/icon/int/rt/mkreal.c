#include "../h/rt.h"

/*
 * mkreal(r, d) - make a descriptor for r.
 * Unstable!
 */

mkreal(r, d)
double r;
register struct descrip *d;
   {
   extern struct b_int *alcreal();

   hneed(sizeof(struct b_real));
   d->type = D_REAL;
   BLKLOC(*d) = alcreal(r);
   }
