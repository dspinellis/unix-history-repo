#include "../h/rt.h"

/*
 * cplist(d1,d2,i,j) - copy sublist d1[i:j] into d2.
 */

cplist(d1, d2, i, j)
struct descrip *d1, *d2;
int i, j;
   {
   register struct descrip *dp;
   int size, nelem;
   struct b_list *lp1, *lp2;
   struct b_lelem *bp1, *bp2;

   /*
    * Calculate the size of the sublist and fail if it's less than 0.
    *  Also round nelem up to the minimum list block size.
    */
   size = nelem = j - i;
   if (size < 0)
      fail();
   if (nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;

   /*
    * Get pointers to the list and list elements for the source list
    *  (bp1, lp1) and the sublist (bp2, lp2).
    */
   hneed(sizeof(struct b_list) + sizeof(struct b_lelem) +
         nelem * sizeof(struct descrip));
   lp1 = (struct b_list *) BLKLOC(*d1);
   bp1 = (struct b_lelem *) BLKLOC(lp1->listhead);
   lp2 = (struct b_list *) alclist(size);
   bp2 = (struct b_lelem *) alclstb(nelem, 0, size);
   lp2->listhead.type = lp2->listtail.type = D_LELEM;
   BLKLOC(lp2->listhead) = BLKLOC(lp2->listtail) = (union block *) bp2;
   dp = bp2->lslots;

   /*
    * Locate the block containing element i in the source list.
    */
   if (size > 0) {
      while (i > bp1->nused) {
         i -= bp1->nused;
         bp1 = (struct b_lelem *) BLKLOC(bp1->listnext);
         }
      }

   /*
    * Copy elements from the source list into the sublist, moving to
    *  the next list block in the source list when all elements in a
    *  block have been copied.
    */
   while (size > 0) {
      j = bp1->first + i - 1;
      if (j >= bp1->nelem)
         j -= bp1->nelem;
      *dp++ = bp1->lslots[j];
      if (++i > bp1->nused) {
         i = 1;
         bp1 = (struct b_lelem *) BLKLOC(bp1->listnext);
         }
      size--;
      }

   /*
    * Fix type and location fields for the new list.
    */
   d2->type = D_LIST;
   BLKLOC(*d2) = (union block *) lp2;
   }
