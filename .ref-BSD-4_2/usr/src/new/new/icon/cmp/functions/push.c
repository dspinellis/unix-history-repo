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
   register struct b_listb *bp;
   extern struct b_listb *alclstb();

   deref(&arg1);
   deref(&arg2);
   if (TYPE(arg1) != T_LIST)
      runerr(108, &arg1);

   hneed(sizeof(struct b_listb)+LISTBLKSIZE*sizeof(struct descrip));

   hp = BLKLOC(arg1);
   bp = BLKLOC(hp->listhead);
   if (bp->nused >= bp->nelem) {
      bp = alclstb(LISTBLKSIZE, 0, 0);
      BLKLOC(hp->listhead)->listprev.type = D_LISTB;
      BLKLOC(BLKLOC(hp->listhead)->listprev) = bp;
      bp->listnext = hp->listhead;
      BLKLOC(hp->listhead) = bp;
      }
   i = bp->first - 1;
   if (i < 0)
      i = bp->nelem - 1;
   bp->lelem[i] = arg2;
   bp->first = i;
   bp->nused++;
   hp->cursize++;
   arg0 = arg1;
   }

struct b_iproc Bpush = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xpush),
   2,
   -1,
   0,
   0,
   {4, "push"}
   };
