#include "../h/rt.h"

/*
 * push(x,val) - push val onto beginning of list x.
 */
Xpush(nargs, arg2, arg1, arg0)
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
    * Point hp at the list header block and bp at the first
    * list element block.
    */
   hp = (struct b_list *) BLKLOC(arg1);
   bp = (struct b_lelem *) BLKLOC(hp->listhead);
   /*
    * If the first list element block is full,
    * allocate a new list element block, make it the first list
    *  element block and make it the previous block of the
    *  former first list element block.
    */
   if (bp->nused >= bp->nelem) {
      bp = alclstb(LISTBLKSIZE, 0, 0);
      BLKLOC(hp->listhead)->lelem.listprev.type = D_LELEM;
      BLKLOC(BLKLOC(hp->listhead)->lelem.listprev) = (union block *) bp;
      bp->listnext = hp->listhead;
      BLKLOC(hp->listhead) = (union block *) bp;
      }
   /*
    * Set i to position of new first element and assign val (arg2) to
    *  that element.
    */
   i = bp->first - 1;
   if (i < 0)
      i = bp->nelem - 1;
   bp->lslots[i] = arg2;
   /*
    * Adjust value of location of first element, block usage count,
    *  and current list size.
    */
   bp->first = i;
   bp->nused++;
   hp->cursize++;
   /*
    * Return the list.
    */
   arg0 = arg1;
   }

Procblock(push,2)
