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

   deref(&arg1);

   if (!QUAL(arg1) && TYPE(arg1) == T_CSET)
      arg0 = arg1;
   else if (cvcset(&arg1, &cs, csbuf) != NULL) {
      arg0.type = D_CSET;
      BLKLOC(arg0) = bp = alccset();
      for (i = 0; i < CSETSIZE; i++)
	 bp->bits[i] = cs[i];
      }
   else
      fail();
   }

struct b_iproc Bcset = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xcset),
   1,
   -1,
   0,
   0,
   {4, "cset"}
   };
