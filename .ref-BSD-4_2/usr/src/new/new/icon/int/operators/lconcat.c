#include "../h/rt.h"

/*
 * x ||| y - concatenate lists x and y.
 */

lconcat(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register struct b_list *bp1, *bp2;
   register struct b_listb *lp1, *lp2;
   int size1, size2;

   SetBound;
   deref(&arg1);
   deref(&arg2);
   if (TYPE(arg1) != T_LIST)
      runerr(108, &arg1);
   if (TYPE(arg2) != T_LIST)
      runerr(108, &arg2);

   size1 = BLKLOC(arg1)->list.cursize;
   size2 = BLKLOC(arg2)->list.cursize;

   cplist(&arg1, &arg1, 1, size1 + 1);
   cplist(&arg2, &arg2, 1, size2 + 1);

   bp1 = BLKLOC(arg1);
   bp2 = BLKLOC(arg2);

   lp1 = BLKLOC(bp1->listtail);
   lp2 = BLKLOC(bp2->listhead);

   lp1->listnext.type = D_LISTB;
   BLKLOC(lp1->listnext) = lp2;

   lp2->listprev.type = D_LISTB;
   BLKLOC(lp2->listprev) = lp1;

   bp1->cursize = size1 + size2;
   BLKLOC(bp1->listtail) = BLKLOC(bp2->listtail);

   arg0 = arg1;
   ClearBound;
   }
struct b_iproc Blconcat = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(lconcat),
   2,
   -1,
   0,
   0,
   {3, "|||"}
   };
