#include "../h/rt.h"

/*
 * x -- y - difference of csets x and y or of sets x and y.
 */

diff(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int i;
   register union block *bp;
   int *cs1, *cs2, csbuf1[CSETSIZE], csbuf2[CSETSIZE];
   extern struct b_cset *alccset();
#ifdef SETS
   struct descrip *dp;
   struct b_set *srcp, *dstp, *tstp;
   struct b_selem *sep;
   extern struct b_set *alcset();
   extern struct b_selem *alcselem();
#endif SETS

   SetBound;
#ifdef SETS
   DeRef(arg1)
   DeRef(arg2)
   if (QUAL(arg1) || QUAL(arg2))
      goto skipsets;
   if (TYPE(arg1) == T_SET && TYPE(arg2) != T_SET)
      runerr(119,&arg2);
   if (TYPE(arg2) == T_SET && TYPE(arg1) != T_SET)
      runerr(119,&arg1);
   if (TYPE(arg1) == T_SET && TYPE(arg2) == T_SET) {
      /*
       * Both x and y are sets - do set difference
       *  get enough space for a new set the size of x.
       */
      hneed(sizeof(struct b_set) + BLKLOC(arg1)->set.setsize *
         sizeof(struct b_selem));
      /*
       * For each element in set x if it isn't in set y
       *  copy it directly into the result set.
       */
      srcp = (struct b_set *) BLKLOC(arg1);
      tstp = (struct b_set *) BLKLOC(arg2);
      arg0.type = D_SET;
      dstp = alcset();
      BLKLOC(arg0) = (union block *) dstp;
      for (i = 0; i < NBUCKETS; i++) {
         sep = (struct b_selem *) BLKLOC(srcp->sbucks[i]);
         dp = &dstp->sbucks[i];
         while (sep != NULL) {
            if ( !locate( BLKLOC(tstp->sbucks[i]), sep) ) {
               dp->type = D_SELEM;
               BLKLOC(*dp) = (union block *) alcselem(&sep->setmem, sep->hnum);
               dp = &BLKLOC(*dp)->selem.sblink;
               dstp->setsize++;
               }
            sep = (struct b_selem *) BLKLOC(sep->sblink);
            }
         }
      }
   else {
      skipsets:
#endif SETS
   hneed(sizeof(struct b_cset));

   /*
    * x and y must be csets.
    */
   if (cvcset(&arg1, &cs1, csbuf1) == NULL)
      runerr(104, &arg1);
   if (cvcset(&arg2, &cs2, csbuf2) == NULL)
      runerr(104, &arg2);

   /*
    * Allocate a new cset and in each word of it, compute the value
    *  of the bitwise difference of the corresponding words in the
    *  x and y csets.
    */
   bp = (union block *) alccset();
   for (i = 0; i < CSETSIZE; i++)
      bp->cset.bits[i] = cs1[i] & ~cs2[i];

   arg0.type = D_CSET;
   BLKLOC(arg0) = bp;
#ifdef SETS
   }
#endif SETS
   ClearBound;
   }

Opblock(diff,2,"--")
