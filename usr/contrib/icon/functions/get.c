#include "../h/rt.h"

/*
 * get(x) - get an element from end of list x.
 * Identical to  pop(x).
 */

Xget(nargs, arg1, arg0)
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
    * Fail if the list is empty.
    */
   hp = (struct b_list *) BLKLOC(arg1);
   if (hp->cursize <= 0)
      fail();

   /*
    * Point bp at the first list block.  If the first block has no
    *  elements in use, point bp at the next list block.
    */
   bp = (struct b_lelem *) BLKLOC(hp->listhead);
   if (bp->nused <= 0) {
      bp = (struct b_lelem *) BLKLOC(bp->listnext);
      BLKLOC(hp->listhead) = (union block *) bp;
      bp->listprev = nulldesc;
      }
   /*
    * Locate first element and assign it to arg0 for return.
    */
   i = bp->first;
   arg0 = bp->lslots[i];
   /*
    * Set bp->first to new first element, or 0 if the block is now
    *  empty.  Decrement the usage count for the block and the size
    *  of the list.
    */
   if (++i >= bp->nelem)
      i = 0;
   bp->first = i;
   bp->nused--;
   hp->cursize--;
   }

Procblock(get,1)
