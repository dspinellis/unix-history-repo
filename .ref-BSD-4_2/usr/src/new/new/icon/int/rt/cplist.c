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
   struct b_listb *bp1, *bp2;

   size = nelem = j - i;
   if (size < 0)
      fail();
   if (nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;

   hneed(sizeof(struct b_list) + sizeof(struct b_listb) +
         nelem * sizeof(struct descrip));
   lp1 = BLKLOC(*d1);
   bp1 = BLKLOC(lp1->listhead);
   lp2 = alclist(size);
   bp2 = alclstb(nelem, 0, size);

   lp2->listhead.type = lp2->listtail.type = D_LISTB;
   BLKLOC(lp2->listhead) = BLKLOC(lp2->listtail) = bp2;
   dp = bp2->lelem;

   /* Find element i in list 1 */

   if (size > 0) {
      while (i > bp1->nused) {
	 i -= bp1->nused;
	 bp1 = BLKLOC(bp1->listnext);
	 }
      }

   /* Copy 'size' elements */

   while (size > 0) {
      j = bp1->first + i - 1;
      if (j >= bp1->nelem)
	 j -= bp1->nelem;
      *dp++ = bp1->lelem[j];
      if (++i > bp1->nused) {
	 i = 1;
	 bp1 = BLKLOC(bp1->listnext);
	 }
      size--;
      }

   d2->type = D_LIST;
   BLKLOC(*d2) = lp2;
   }
