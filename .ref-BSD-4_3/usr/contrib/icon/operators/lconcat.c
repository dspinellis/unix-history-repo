#include "../h/rt.h"

/*
 * x ||| y - concatenate lists x and y.
 */

lconcat(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register struct b_list *bp1, *bp2;
   register struct b_lelem *lp1, *lp2;
   int size1, size2;

   SetBound;
   /*
    * x and y must be lists.
    */
   DeRef(arg1)
   DeRef(arg2)
   if (QUAL(arg1) || TYPE(arg1) != T_LIST)
      runerr(108, &arg1);
   if (QUAL(arg2) || TYPE(arg2) != T_LIST)
      runerr(108, &arg2);

   /*
    * Get the size of both lists.
    */
   size1 = BLKLOC(arg1)->list.cursize;
   size2 = BLKLOC(arg2)->list.cursize;

   /*
    * Make a copy of both lists.
    */
   cplist(&arg1, &arg1, 1, size1 + 1);
   cplist(&arg2, &arg2, 1, size2 + 1);

   /*
    * Get a pointer to both lists.  bp1 points to the copy of x and is
    *  the list that will be returned.
    */
   bp1 = (struct b_list *) BLKLOC(arg1);
   bp2 = (struct b_list *) BLKLOC(arg2);

   /*
    * Perform the concatenation by hooking the lists together so
    *  that the next list of x is y and the previous list of y is x.
    */
   lp1 = (struct b_lelem *) BLKLOC(bp1->listtail);
   lp2 = (struct b_lelem *) BLKLOC(bp2->listhead);

   lp1->listnext.type = D_LELEM;
   BLKLOC(lp1->listnext) = (union block *) lp2;

   lp2->listprev.type = D_LELEM;
   BLKLOC(lp2->listprev) = (union block *) lp1;

   /*
    * Adjust the size field to reflect the length of the concatenated lists.
    */
   bp1->cursize = size1 + size2;
   BLKLOC(bp1->listtail) = BLKLOC(bp2->listtail);

   arg0 = arg1;
   ClearBound;
   }

Opblock(lconcat,2,"|||")
