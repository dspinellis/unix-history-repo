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
   register struct b_listb *bp;
   extern struct b_listb *alclstb();

   deref(&arg1);
   if (TYPE(arg1) != T_LIST)
      runerr(108, &arg1);

   hp = BLKLOC(arg1);
   if (hp->cursize <= 0)
      fail();
   bp = BLKLOC(hp->listtail);
   if (bp->nused <= 0) {
      bp = BLKLOC(bp->listprev);
      BLKLOC(hp->listtail) = bp;
      bp->listnext = nulldesc;
      }
   i = bp->first + bp->nused - 1;
   if (i >= bp->nelem)
      i -= bp->nelem;
   arg0 = bp->lelem[i];
   bp->nused--;
   hp->cursize--;
   }

struct b_iproc Bpull = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xpull),
   1,
   -1,
   0,
   0,
   {4, "pull"}
   };
