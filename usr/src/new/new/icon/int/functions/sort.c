#include "../h/rt.h"

/*
 * sort(l) - sort list l.
 * sort(t,i) - sort table on reference (i = 1) or value (i = 2) field.
 */

Xsort(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register struct descrip *d1, *dp;
   register int size, i, j;
   int nelem;
   struct b_list *lp, *tp;
   union block *bp, *ep;
   long l;
   extern struct b_list *alclist();
   extern struct b_listb *alclstb();
   extern anycmp(), trefcmp(), tvalcmp();

   deref(&arg1);
   if (arg1.type == D_LIST) {
      size = BLKLOC(arg1)->list.cursize;
      cplist(&arg1, &arg0, 1, size + 1);
      qsort(BLKLOC(BLKLOC(arg0)->list.listhead)->listb.lelem, size,
            sizeof(struct descrip), anycmp);
      }
   else if (arg1.type == D_TABLE) {
      defshort(&arg2, 1);
      if (arg2.value.integer != 1 && arg2.value.integer != 2)
         runerr(205, &arg2);
      nelem = size = BLKLOC(arg1)->table.cursize;
      if (nelem < LISTBLKSIZE)
	 nelem = LISTBLKSIZE;
      hneed(sizeof(struct b_list) + sizeof(struct b_listb) +
	    nelem * (sizeof(struct b_list) + sizeof(struct b_listb) +
		     3 * sizeof(struct descrip)));
      bp = BLKLOC(arg1);
      lp = alclist(size);
      lp->listhead.type = lp->listtail.type = D_LISTB;
      BLKLOC(lp->listhead) = BLKLOC(lp->listtail) = alclstb(nelem, 0, size);
      if (size > 0) {
         d1 = BLKLOC(lp->listhead)->listb.lelem;
         for (i = 0; i < NBUCKETS; i++) {
            ep = BLKLOC(bp->table.buckets[i]);
            while (ep != NULL) {
               d1->type = D_LIST;
               BLKLOC(*d1) = tp = alclist(2);
               tp->listhead.type = tp->listtail.type = D_LISTB;
               BLKLOC(tp->listhead) = BLKLOC(tp->listtail) = alclstb(2, 0, 2);
               BLKLOC(tp->listhead)->listb.lelem[0] = ep->telem.tref;
               BLKLOC(tp->listhead)->listb.lelem[1] = ep->telem.tval;
               d1++;
               ep = BLKLOC(ep->telem.blink);
               }
            }
         if (arg2.value.integer == 1)
            qsort(BLKLOC(lp->listhead)->listb.lelem, size,
                  sizeof(struct descrip), trefcmp);
         else
            qsort(BLKLOC(lp->listhead)->listb.lelem, size,
                  sizeof(struct descrip), tvalcmp);
         }
      arg0.type = D_LIST;
      BLKLOC(arg0) = lp;
      }
   else
      runerr(115, &arg1);
   }

/*
 * trefcmp(d1,d2) - compare two element lists on first field.
 */

trefcmp(d1,d2)
struct descrip *d1, *d2;
   {
   extern anycmp();

   if (d1->type != D_LIST || d2->type != D_LIST)
      syserr("trefcmp: internal consistency check fails.");
   return (anycmp(&(BLKLOC(BLKLOC(*d1)->list.listhead)->listb.lelem[0]),
                  &(BLKLOC(BLKLOC(*d2)->list.listhead)->listb.lelem[0])));
   }

/*
 * tvalcmp(d1,d2) - compare two element lists on second field.
 */

tvalcmp(d1,d2)
struct descrip *d1, *d2;
   {
   extern anycmp();

   if (d1->type != D_LIST || d2->type != D_LIST)
      syserr("tvalcmp: internal consistency check fails.");
   return (anycmp(&(BLKLOC(BLKLOC(*d1)->list.listhead)->listb.lelem[1]),
                  &(BLKLOC(BLKLOC(*d2)->list.listhead)->listb.lelem[1])));
   }

struct b_iproc Bsort = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xsort),
   2,
   -1,
   0,
   0,
   {4, "sort"}
   };
