#include "../h/rt.h"

/*
 * ~x - complement cset x.
 */

compl(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   register int i;
   union block *bp;
   int *cs, csbuf[CSETSIZE];
   extern struct b_cset *alccset();

   SetBound;
   hneed(sizeof(struct b_cset));

   if (cvcset(&arg1, &cs, csbuf) == NULL)
      runerr(104, &arg1);

   bp = alccset();
   for (i = 0; i < CSETSIZE; i++)
       bp->cset.bits[i] = ~cs[i];
   arg0.type = D_CSET;
   BLKLOC(arg0) = bp;
   ClearBound;
   }
struct b_iproc Bcompl = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(compl),
   1,
   -1,
   0,
   0,
   {1, "~"}
   };
