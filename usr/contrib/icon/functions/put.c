#include "../h/rt.h"

/*
 * put(x,val) - put val onto end of list x.
 */
Xput(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int i;
   register struct b_list *hp;
   register struct b_lelem *bp;
   extern struct b_lelem *alclstb();

   /*
    * x must be a list.
    */
   DeRef(arg1)
   DeRef(arg2)
   if (QUAL(arg1) || TYPE(arg1) != T_LIST)
      runerr(108, &arg1);

   /*
    * A new list element block might be needed, so ensure space for it.
    */
   hneed(sizeof(struct b_lelem)+LISTBLKSIZE*sizeof(struct descrip));

   /*
    * Point hp at the list header block and bp at the last list element block.
    */
   hp = (struct b_list *) BLKLOC(arg1);
   bp = (struct b_lelem *) BLKLOC(hp->listtail);

   /*
    * If the last list element block is full,
    *  allocate a new list element block, make it the first list
    *  element block and it make it the next block of the
    *  former last list element block.
    */
   if (bp->nused >= bp->nelem) {
      bp = alclstb(LISTBLKSIZE, 0, 0);
      BLKLOC(hp->listtail)->lelem.listnext.type = D_LELEM;
      BLKLOC(BLKLOC(hp->listtail)->lelem.listnext) = (union block *) bp;
      bp->listprev = hp->listtail;
      BLKLOC(hp->listtail) = (union block *) bp;
      }
   /*
    * Set i to position of new last element and assign val (arg2) to
    *  that element.
    */
   i = bp->first + bp->nused;
   if (i >= bp->nelem)
      i -= bp->nelem;
   bp->lslots[i] = arg2;
   /*
    * Adjust block usage count and current list size.
    */
   bp->nused++;
   hp->cursize++;
   /*
    * Return the list.
    */
   arg0 = arg1;
   }

Procblock(put,2)
