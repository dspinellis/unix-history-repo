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
   register struct b_listb *bp;
   extern struct b_listb *alclstb();

   deref(&arg1);
   deref(&arg2);
   if (TYPE(arg1) != T_LIST)
      runerr(108, &arg1);

   hneed(sizeof(struct b_listb)+LISTBLKSIZE*sizeof(struct descrip));

   hp = BLKLOC(arg1);
   bp = BLKLOC(hp->listtail);
   if (bp->nused >= bp->nelem) {
      bp = alclstb(LISTBLKSIZE, 0, 0);
      BLKLOC(hp->listtail)->listnext.type = D_LISTB;
      BLKLOC(BLKLOC(hp->listtail)->listnext) = bp;
      bp->listprev = hp->listtail;
      BLKLOC(hp->listtail) = bp;
      }
   i = bp->first + bp->nused;
   if (i >= bp->nelem)
      i -= bp->nelem;
   bp->lelem[i] = arg2;
   bp->nused++;
   hp->cursize++;
   arg0 = arg1;
   }

struct b_iproc Bput = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xput),
   2,
   -1,
   0,
   0,
   {3, "put"}
   };
