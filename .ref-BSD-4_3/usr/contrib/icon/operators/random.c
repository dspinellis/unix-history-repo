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
   double r1;
   char sbuf[MAXSTRING];
   union block *ep;
   struct descrip *dp;
   extern char *alcstr();

   SetBound;
   arg1v = arg1;
   DeRef(arg1)

   /*
    * x must not be null.
    */
   if (NULLDESC(arg1))
      runerr(113, &arg1);

   if (QUAL(arg1)) {
      /*
       * x is a string, produce a random character in it as the result.
       *  Note that a substring trapped variable is returned.
       */
      if ((val = STRLEN(arg1)) <= 0)
         fail();
      hneed(sizeof(struct b_tvsubs));
      mksubs(&arg1v, &arg1, (int)(randval*val)+1, 1, &arg0);
      ClearBound;
      return;
      }

   switch (TYPE(arg1)) {
      case T_CSET:
         /*
          * x is a cset.  Convert it to a string, select a random character
          *  of that string and return it.  Note that a substring trapped
          *  variable is not needed.
          */
         cvstr(&arg1, sbuf);
         if ((val = STRLEN(arg1)) <= 0)
            fail();
         sneed(1);
         STRLEN(arg0) = 1;
         STRLOC(arg0) = alcstr(STRLOC(arg1)+(int)(randval*val), 1);
         ClearBound;
         return;

      case T_REAL:
         /*
          * x is real.  Convert it to an integer and be sure that it is
          *  non-negative and less than MAXSHORT.  Jump to common code
          *  to compute a random value.  Note that reals are functionally
          *  equivalent to integers.
          */
         r1 = BLKLOC(arg1)->realblk.realval;
         if (r1 < 0 || r1 > MAXSHORT)
            runerr(205, &arg1);
         val = (int)r1;
         goto getrand;

      case T_INTEGER:
         /*
          * x is an integer, be sure that it's non-negative.
          */
         val = INTVAL(arg1);
         if (val < 0)
            runerr(205, &arg1);
      getrand:
         /*
          * val contains the integer value of x.  If val is 0, return
          *  a real in the range [0,1), else return an integer in the
          *  range [1,val].
          */
         if (val == 0)
            mkreal(randval, &arg0);
         else
            mkint((long)(randval*val) + 1, &arg0);
         ClearBound;
         return;

#ifdef LONGS
      case T_LONGINT:
         /*
          * Produce an error if x is a long integer.
          */
         runerr(205, &arg1);
#endif LONGS
      case T_LIST:
         /*
          * x is a list.  Set i to a random number in the range [1,*x],
          *  failing if the list is empty.
          */
         bp = BLKLOC(arg1);
         val = bp->list.cursize;
         if (val <= 0)
            fail();
         i = (int)(randval*val) + 1;
            j = 1;
         /*
          * Work down chain list of list blocks and find the block that
          *  contains the selected element.
          */
            bp = BLKLOC(BLKLOC(arg1)->list.listhead);
            while (i >= j + bp->lelem.nused) {
               j += bp->lelem.nused;
               if (TYPE(bp->lelem.listnext) != T_LELEM)
               syserr("list reference out of bounds in random");
               bp = BLKLOC(bp->lelem.listnext);
            }
         /*
          * Locate the appropriate element and return a variable 
          * that points to it.
          */
            i += bp->lelem.first - j;
         if (i >= bp->lelem.nelem)
            i -= bp->lelem.nelem;
         dp = &bp->lelem.lslots[i];
         arg0.type = D_VAR + ((int *)dp - (int *)bp);
         VARLOC(arg0) = dp;
         ClearBound;
            return;

      case T_TABLE:
          /*
           * x is a table.  Set i to a random number in the range [1,*x],
           *  failing if the table is empty.
           */
          bp = BLKLOC(arg1);
          val = bp->table.cursize;
          if (val <= 0)
             fail();
          i = (int)(randval*val) + 1;
          /*
           * Work down the chain of elements in each bucket and return
           *  a variable that points to the i'th element encountered.
           */
          for (j = 0; j < NBUCKETS; j++) {
             for (ep = BLKLOC(bp->table.buckets[j]); ep != NULL;
                     ep = BLKLOC(ep->telem.blink)) {
                if (--i <= 0) {
                   dp = &ep->telem.tval;
                   arg0.type = D_VAR + ((int *)dp - (int *)bp);
                   VARLOC(arg0) = dp;
                   ClearBound;
                   return;
                   }
                }
             }
#ifdef SETS
      case T_SET:
         /*
          * x is a set.  Set i to a random number in the range [1,*x],
          *  failing if the set is empty.
          */
         bp = BLKLOC(arg1);
         val = bp->set.setsize;
         if (val <= 0)
            fail();
         i = (int)(randval*val) + 1;
         /*
          * Work down the chain of elements in each bucket and return
          *  the value of the i'th element encountered.
          */
         for (j = 0; j < NBUCKETS; j++) {
            for (ep = BLKLOC(bp->set.sbucks[j]); ep != NULL;
               ep = BLKLOC(ep->selem.sblink)) {
                  if (--i <= 0) {
                     arg0 = ep->selem.setmem;
                     ClearBound;
                     return;
                     }
                 }   
             }
#endif SETS

      case T_RECORD:
         /*
          * x is a record.  Set val to a random number in the range [1,*x]
          *  (*x is the number of fields), failing if the record has no
          *  fields.
          */
         bp = BLKLOC(arg1);
         val = bp->record.recptr->nfields;
         if (val <= 0)
            fail();
         /*
          * Locate the selected element and return a variable
          * that points to it
          */
            dp = &bp->record.fields[(int)(randval*val)];
              arg0.type = D_VAR + ((int *)dp - (int *)bp);
         VARLOC(arg0) = dp;
         ClearBound;
            return;

      default:
         /*
          * x is of a type for which there is no notion of elements.
          */
         runerr(113, &arg1);
      }
   }

Opblockx(random,2,"?",1)
