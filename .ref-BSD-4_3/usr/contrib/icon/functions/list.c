#include "../h/rt.h"

/*
 * list(n,x) - create a list of size n, with initial value x.
 */

Xlist(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int i, size;
   register struct b_lelem *bp;
   register struct b_list *hp;
   int nelem;
   extern struct b_list *alclist();
   extern struct b_lelem *alclstb();

   defshort(&arg1, 0);	/* Size defaults to 0 */
   DeRef(arg2)

   nelem = size = INTVAL(arg1);
   /*
    * Ensure that the size is positive and that the list element block will
    *  have at least LISTBLKSIZE element slots.
    */
   if (size < 0)
      runerr(205, &arg1);
   if (nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;

   /*
    * Ensure space for a list header block, and a list element block
    * with nelem element slots.
    */
   hneed(sizeof(struct b_list) + sizeof(struct b_lelem) +
         nelem * sizeof(struct descrip));

   /*
    * Allocate the list header block and a list element block.
    *  Note that nelem is not equivalent to size
    *  because nelem is the number of elements in the list element
    *  block while size is the number of elements in the
    *  list.
    */
   hp = alclist(size);
   bp = alclstb(nelem, 0, size);
   hp->listhead.type = hp->listtail.type = D_LELEM;
   BLKLOC(hp->listhead) = BLKLOC(hp->listtail) = (union block *) bp;
   /*
    * Initialize each list element.
    */
   for (i = 0; i < size; i++)
      bp->lslots[i] = arg2;
   /*
    * Return the new list.
    */
   arg0.type = D_LIST;
   BLKLOC(arg0) = (union block *) hp;
   }

Procblock(list,2)
