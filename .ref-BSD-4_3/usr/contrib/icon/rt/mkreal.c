#include "../h/rt.h"

/*
 * mkreal(r, d) - make a real number descriptor and associated heap block
 *  for r and place it in *d.
 */

mkreal(r, d)
double r;
register struct descrip *d;
   {
   extern struct b_int *alcreal();

   hneed(sizeof(struct b_real));
   d->type = D_REAL;
   BLKLOC(*d) = (union block *) alcreal(r);
   }
