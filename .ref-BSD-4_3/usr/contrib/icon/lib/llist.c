#include "../h/rt.h"

/*
 * [ ... ] - create an explicitly specified list.
 */

llist(nargs)
int nargs;
   {
   register int i;
   register struct b_list *hp;
   register struct b_lelem *bp;
   extern struct b_list *alclist();
   extern struct b_lelem *alclstb();
   int nelem;

   SetBound;

   /*
    * Round the number of elements in the list (as indicated by nargs)
    *  up to LISTBLKSIZE and ensure space for the list.
    */
   nelem = nargs;
   if (nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;
   hneed(sizeof(struct b_list) + sizeof(struct b_lelem) +
         nelem * sizeof(struct descrip));

   /*
    * Allocate the list and a list block.
    */
   hp = alclist(nargs);
   bp = alclstb(nelem, 0, nargs);

   /*
    * Make the list block just allocated into the first and last blocks
    *  for the list.
    */
   hp->listhead.type = hp->listtail.type = D_LELEM;
   BLKLOC(hp->listhead) = BLKLOC(hp->listtail) = (union block *) bp;
   /*
    * Dereference each argument in turn and assign it to a list element.
    */
   for (i = 1; i <= nargs; i++) {
      DeRef(ARG(i))
      bp->lslots[i-1] = ARG(i);
      }
   /*
    * Point arg0 at the new list and return it.
    */
   ARGTYPE(0) = D_LIST;
   ARGVAL(0) = (int) hp;
   ClearBound;
   }
