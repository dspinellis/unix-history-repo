#include "../h/rt.h"

/*
 * pop(x) - pop an element from beginning of list x.
 */

Xpop(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int i;
   register struct b_list *hp;
   register struct b_listb *bp;
   extern struct b_listb *alclstb();

   deref(&arg1);
   if (TYPE(arg1) != T_LIST)
      runerr(108, &arg1);

   hp = BLKLOC(arg1);
   if (hp->cursize <= 0)
      fail();
   bp = BLKLOC(hp->listhead);
   if (bp->nused <= 0) {
      bp = BLKLOC(bp->listnext);
      BLKLOC(hp->listhead) = bp;
      bp->listprev = nulldesc;
      }
   i = bp->first;
   arg0 = bp->lelem[i];
   if (++i >= bp->nelem)
      i = 0;
   bp->first = i;
   bp->nused--;
   hp->cursize--;
   }

struct b_iproc Bpop = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xpop),
   1,
   -1,
   0,
   0,
   {3, "pop"}
   };
