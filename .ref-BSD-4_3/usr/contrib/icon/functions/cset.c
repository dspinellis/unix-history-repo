#include "../h/rt.h"

/*
 * cset(x) - convert x to cset.
 */

Xcset(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;
   register struct b_cset *bp;
   int *cs, csbuf[CSETSIZE];
   extern struct b_cset *alccset();

   hneed(sizeof(struct b_cset));

   DeRef(arg1)

   if (!QUAL(arg1) && TYPE(arg1) == T_CSET)
      /*
       * x is already a cset, just return it.
       */
      arg0 = arg1;
   else if (cvcset(&arg1, &cs, csbuf) != NULL) {
      /*
       * x was convertible to cset and the result resides in csbuf.  Allocate
       *  a cset, make arg0 a descriptor for it and copy the bits from csbuf
       *  into it.
       */
      arg0.type = D_CSET;
      bp = alccset();
      BLKLOC(arg0) =  (union block *) bp;
      for (i = 0; i < CSETSIZE; i++)
         bp->bits[i] = cs[i];
      }
   else /* Not a cset nor convertible to one. */
      fail();
   }

Procblock(cset,1)
