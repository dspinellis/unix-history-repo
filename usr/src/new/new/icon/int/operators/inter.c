#include "../h/rt.h"

/*
 * x ** y - intersection of csets x and y.
 */

inter(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   DclSave
   register int i;
   union block *bp;
   int *cs1, csbuf1[CSETSIZE], *cs2, csbuf2[CSETSIZE];
   extern struct b_cset *alccset();

   SetBound;
   hneed(sizeof(struct b_cset));

   if (cvcset(&arg1, &cs1, csbuf1) == NULL)
      runerr(104, &arg1);
   if (cvcset(&arg2, &cs2, csbuf2) == NULL)
      runerr(104, &arg2);

   bp = alccset();
   for (i = 0; i < CSETSIZE; i++)
      bp->cset.bits[i] = cs1[i] & cs2[i];

   arg0.type = D_CSET;
   BLKLOC(arg0) = bp;
   ClearBound;
   }
struct b_iproc Binter = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(inter),
   2,
   -1,
   0,
   0,
   {2, "**"}
   };
