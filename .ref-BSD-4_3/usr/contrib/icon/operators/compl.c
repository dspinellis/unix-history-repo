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

   /*
    * x must be a cset.
    */
   if (cvcset(&arg1, &cs, csbuf) == NULL)
      runerr(104, &arg1);

   /*
    * Allocate a new cset and then copy each cset word from x into
    *  the new cset words, complementing each.
    */
   bp = (union block *) alccset();
   for (i = 0; i < CSETSIZE; i++)
       bp->cset.bits[i] = ~cs[i];

   arg0.type = D_CSET;
   BLKLOC(arg0) = bp;
   ClearBound;
   }

Opblock(compl,1,"~")
