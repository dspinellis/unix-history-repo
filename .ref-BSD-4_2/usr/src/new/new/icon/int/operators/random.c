#include "../h/rt.h"
#include "../h/record.h"
#define randval (RSCALE*(k_random=(RANDA*k_random+RANDC)&MAXLONG))

/*
 * ?x - produce a randomly selected element of x.
 */

random(nargs, arg1v, arg1, arg0)
int nargs;
struct descrip arg1v, arg1, arg0;
   {
   register int val, i, j;
   register union block *bp;
   long l1;
   double r1;
   char sbuf[MAXSTRING];
   union block *ep;
   struct descrip *dp;
   extern char *alcstr();

   SetBound;
   arg1v = arg1;
   deref(&arg1);

   if (NULLDESC(arg1))
      runerr(113, &arg1);

   if (QUAL(arg1)) {                    /* random char in string */
      if ((val = STRLEN(arg1)) <= 0)
         fail();
      hneed(sizeof(struct b_tvsubs));
      mksubs(&arg1v, &arg1, (int)(randval*val)+1, 1, &arg0);
      ClearBound;
      return;
      }

   switch (TYPE(arg1)) {
      case T_CSET:
         cvstr(&arg1, sbuf);
         if ((val = STRLEN(arg1)) <= 0)
            fail();
         sneed(1);
         STRLEN(arg0) = 1;
         STRLOC(arg0) = alcstr(STRLOC(arg1)+(int)(randval*val), 1);
         ClearBound;
         return;

      case T_REAL:
         r1 = BLKLOC(arg1)->realval;
         if (r1 < 0 || r1 > MAXSHORT)
            runerr(205, &arg1);
         val = (int)r1;
         goto getrand;

      case T_INTEGER:
         val = INTVAL(arg1);
         if (val < 0)
            runerr(205, &arg1);
      getrand:
         if (val == 0)          /* return real in range [0,1) */
            mkreal(randval, &arg0);
         else                   /* return integer in range [1,val] */
            mkint((long)(randval*val) + 1, &arg0);
         ClearBound;
         return;

#ifndef BIT32
      case T_LONGINT:
         runerr(205, &arg1);

#endif
      case T_LIST:
         bp = BLKLOC(arg1);
         val = bp->list.cursize;
         if (val <= 0)
	    fail();
         i = (int)(randval*val) + 1;
   	 j = 1;
   	 bp = BLKLOC(BLKLOC(arg1)->list.listhead);
   	 while (i >= j + bp->listb.nused) {
   	    j += bp->listb.nused;
   	    if (TYPE(bp->listb.listnext) != T_LISTB)
	       syserr("list reference out of bounds in random");
   	    bp = BLKLOC(bp->listb.listnext);
	    }
   	 i += bp->listb.first - j;
	 if (i >= bp->listb.nelem)
	    i -= bp->listb.nelem;
	 dp = &bp->listb.lelem[i];
   	 arg0.type = D_VAR + ((int *)dp - (int *)bp);
   	 BLKLOC(arg0) = dp;
         ClearBound;
   	 return;

      case T_TABLE:
          bp = BLKLOC(arg1);
          val = bp->table.cursize;
          if (val <= 0)
	     fail();
          i = (int)(randval*val) + 1;
          for (j = 0; j < NBUCKETS; j++) {
             for (ep = BLKLOC(bp->table.buckets[j]); ep != NULL;
   	          ep = BLKLOC(ep->telem.blink)) {
                if (--i <= 0) {
                   dp = &ep->telem.tval;
                   arg0.type = D_VAR + ((int *)dp - (int *)bp);
                   BLKLOC(arg0) = dp;
                   ClearBound;
                   return;
                   }
                }
             }

      case T_RECORD:
         bp = BLKLOC(arg1);
         val = bp->record.recptr->nfields;
         if (val <= 0)
	    fail();
   	 dp = &bp->record.fields[(int)(randval*val)];
     	 arg0.type = D_VAR + ((int *)dp - (int *)bp);
         BLKLOC(arg0) = dp;
         ClearBound;
   	 return;

      default:
         runerr(113, &arg1);
      }
   }
struct b_iproc Brandom = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(random),
   2,
   -1,
   -1,
   0,
   {1, "?"}
   };
