#include "../h/rt.h"

/*
 * pull(x) - pull an element from end of list x.
 */

Xpull(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;
   register struct b_list *hp;
   register struct b_lelem *bp;
   extern struct b_lelem *alclstb();

   /*
    * x must be a list.
    */
   DeRef(arg1)
   if (QUAL(arg1) || TYPE(arg1) != T_LIST)
      runerr(108, &arg1);

   /*
    * Point at list header block and fail if the list is empty.
    */
   hp = (struct b_list *) BLKLOC(arg1);
   if (hp->cursize <= 0)
      fail();
   /*
    * Point bp at the last list element block.  If the last block has no
    *  elements in use, point bp at the previous list element block.
    */
   bp = (struct b_lelem *) BLKLOC(hp->listtail);
   if (bp->nused <= 0) {
      bp = (struct b_lelem *) BLKLOC(bp->listprev);
      BLKLOC(hp->listtail) = (union block *) bp;
      bp->listnext = nulldesc;
      }
   /*
    * Set i to position of last element and assign the element to
    *  arg0 for return.  Decrement the usage count for the block
    *  and the size of the list.
    */
   i = bp->first + bp->nused - 1;
   if (i >= bp->nelem)
      i -= bp->nelem;
   arg0 = bp->lslots[i];
   bp->nused--;
   hp->cursize--;
   }

Procblock(pull,1)
