#include "../h/rt.h"

/*
 * sort(l) - sort list l.
 * sort(S) - sort set S.
 * sort(t,i) - sort table on reference (i = 1) or value (i = 2) field.
 */

Xsort(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register struct descrip *d1;
   register int size, i;
   int nelem;
   struct b_list *lp, *tp;
   union block *bp, *ep;
   extern struct b_list *alclist();
   extern struct b_lelem *alclstb();
   extern anycmp(), trefcmp(), tvalcmp();

   DeRef(arg1)
   if (arg1.type == D_LIST) {
      /*
       * Sort the list by copying it into a new list and then using
       *  qsort to sort the descriptors.  (That was easy!)
       */
      size = BLKLOC(arg1)->list.cursize;
      cplist(&arg1, &arg0, 1, size + 1);
      qsort(BLKLOC(BLKLOC(arg0)->list.listhead)->lelem.lslots, size,
            sizeof(struct descrip), anycmp);
      }
#ifdef SETS
   else if (arg1.type == D_SET) {
      /*
       * Create a list the size of the set (or at least 
       *  LISTBLKSIZE), copy each element into the list, and
       *  then sort the list using qsort as in list sorting
       *  and return the sorted list.
       */
   nelem = size = BLKLOC(arg1)->set.setsize;
   if(nelem < LISTBLKSIZE)
      nelem = LISTBLKSIZE;
   hneed(sizeof(struct b_list) + sizeof(struct b_lelem) +
      nelem * sizeof(struct descrip));

   bp = BLKLOC(arg1);
   lp = alclist(size);
   lp->listhead.type = lp->listtail.type = D_LELEM;
      BLKLOC(lp->listtail) = (union block *) alclstb(nelem,0,size);
   BLKLOC(lp->listhead) = BLKLOC(lp->listtail);
   if (size > 0) {  /* only need to sort non-empty sets */
      d1 = BLKLOC(lp->listhead)->lelem.lslots;
      for(i = 0; i < NBUCKETS; i++) {
      ep = BLKLOC(bp->set.sbucks[i]);
      while (ep != NULL) {
         *d1 = ep->selem.setmem;
         d1++;
         ep = BLKLOC(ep->selem.sblink);
         }
      }
      qsort(BLKLOC(lp->listhead)->lelem.lslots,size,sizeof(struct descrip),anycmp);
   }
   arg0.type = D_LIST;
   BLKLOC(arg0) = (union block *) lp;
   }
#endif SETS

   else if (arg1.type == D_TABLE) {
      /*
       * Default i (the type of sort) to 1, and be sure that it is
       *  either 1 or 2.
       */
      defshort(&arg2, 1);
      if (INTVAL(arg2) != 1 && INTVAL(arg2) != 2)
         runerr(205, &arg2);

      /*
       * The list resulting from the sort will have as many elements as
       *  the table has, so get that value and also make a valid list
       *  block size out of it.
       */
      nelem = size = BLKLOC(arg1)->table.cursize;
      if (nelem < LISTBLKSIZE)
         nelem = LISTBLKSIZE;
      /*
       * Ensure space for: the list header block and a list element
       *  block for the list which is to be returned,
       *  a list header block and a list element block for each of the two
       *  element lists the sorted list is to contain.  Note that the
       *  calculation might be better expressed as:
       *    list_header_size + list_block_size + nelem * descriptor_size +
       *     nelem * (list_header_size + list_block_size + 2*descriptor_size)
       */
      hneed(sizeof(struct b_list) + sizeof(struct b_lelem) +
         nelem * (sizeof(struct b_list) + sizeof(struct b_lelem) +
            3 * sizeof(struct descrip)));
      /*
       * Point bp at the table header block of the table to be sorted
       *  and point lp at a newly allocated list
       *  that will hold the the result of sorting the table.
       */
      bp = BLKLOC(arg1);
      lp = alclist(size);
      lp->listhead.type = lp->listtail.type = D_LELEM;
      BLKLOC(lp->listtail) = (union block *) alclstb(nelem, 0, size);
      BLKLOC(lp->listhead) = BLKLOC(lp->listtail);
      if (size > 0) { /* No need to sort the elements of an empty table */
         /*
          * Point d1 at the start of the list elements in the new list
          *  element block in preparation for use as an index into the list.
          */
         d1 = BLKLOC(lp->listhead)->lelem.lslots;
         /*
          * Traverse the element chain for each table bucket.  For each
          *  element, allocate a two-element list and put the table
          *  entry value in the first element and the assigned value in
          *  the second element.  The two-element list is assigned to
          *  the descriptor that d1 points at.  When this is done, the
          *  list of two-element lists is complete, but unsorted.
          */
         for (i = 0; i < NBUCKETS; i++) {
            ep = BLKLOC(bp->table.buckets[i]);
            while (ep != NULL) {
               d1->type = D_LIST;
               tp = alclist(2);
               BLKLOC(*d1) = (union block *) tp;
               tp->listhead.type = tp->listtail.type = D_LELEM;
               BLKLOC(tp->listtail) = (union block *) alclstb(2, 0, 2);
               BLKLOC(tp->listhead) = BLKLOC(tp->listtail);
               BLKLOC(tp->listhead)->lelem.lslots[0] = ep->telem.tref;
               BLKLOC(tp->listhead)->lelem.lslots[1] = ep->telem.tval;
               d1++;
               ep = BLKLOC(ep->telem.blink);
               }
            }
         /*
          * Sort the resulting two-element list using the sorting function
          *  determined by i.
          */
         if (INTVAL(arg2) == 1)
            qsort(BLKLOC(lp->listhead)->lelem.lslots, size,
                  sizeof(struct descrip), trefcmp);
         else
            qsort(BLKLOC(lp->listhead)->lelem.lslots, size,
                  sizeof(struct descrip), tvalcmp);
         }
      /*
       * Make arg0 point at the sorted list.
       */
      arg0.type = D_LIST;
      BLKLOC(arg0) = (union block *) lp;
      }
   else /* Tried to sort something that wasn't a list or a table. */
      runerr(115, &arg1);
   }

Procblock(sort,2)

/*
 * trefcmp(d1,d2) - compare two-element lists on first field.
 */

trefcmp(d1,d2)
struct descrip *d1, *d2;
   {
   extern anycmp();

   if (d1->type != D_LIST || d2->type != D_LIST)
      syserr("trefcmp: internal consistency check fails.");
   return (anycmp(&(BLKLOC(BLKLOC(*d1)->list.listhead)->lelem.lslots[0]),
                  &(BLKLOC(BLKLOC(*d2)->list.listhead)->lelem.lslots[0])));
   }

/*
 * tvalcmp(d1,d2) - compare two-element lists on second field.
 */

tvalcmp(d1,d2)
struct descrip *d1, *d2;
   {
   extern anycmp();

   if (d1->type != D_LIST || d2->type != D_LIST)
      syserr("tvalcmp: internal consistency check fails.");
   return (anycmp(&(BLKLOC(BLKLOC(*d1)->list.listhead)->lelem.lslots[1]),
                  &(BLKLOC(BLKLOC(*d2)->list.listhead)->lelem.lslots[1])));
   }
