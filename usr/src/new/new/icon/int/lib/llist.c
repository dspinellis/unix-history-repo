#include "../h/rt.h"

/*
 * [ ... ] - create a literal list.
 */

llist(nargs)
int nargs;
   {
   register int i;
   register struct b_list *hp;
   register struct b_listb *bp;
   int nelem;

   SetBound;

   nelem = nargs;
   if (nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;
   hneed(sizeof(struct b_list) + sizeof(struct b_listb) +
         nelem * sizeof(struct descrip));

   hp = alclist(nargs);
   bp = alclstb(nelem, 0, nargs);

   hp->listhead.type = hp->listtail.type = D_LISTB;
   BLKLOC(hp->listhead) = BLKLOC(hp->listtail) = bp;
   for (i = 1; i <= nargs; i++) {
      deref(&ARG(i));
      bp->lelem[i-1] = ARG(i);
      }
   ARGTYPE(0) = D_LIST;
   ARGVAL(0) = hp;
   ClearBound;
   }
