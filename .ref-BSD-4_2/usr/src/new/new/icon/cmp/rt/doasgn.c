#include "../h/rt.h"

/*
 * doasgn - assign value of a2 to variable a1.
 * Does the work for asgn, swap, rasgn, and rswap.
 */

doasgn(a1, a2)
struct descrip *a1, *a2;
   {
   register int l1, l2;
   register union block *bp;
   register struct b_table *tp;
   long l3;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern struct descrip tended[];   /* uses tended[1] thru tended[5] */
   extern struct b_listb *alclstb();
   extern struct b_telem *alctelem();

   tended[1] = *a1;
   tended[2] = *a2;

assign:
   if (QUAL(tended[1]) || !VAR(tended[1]))
      syserr("doasgn: variable expected");

   if (TVAR(tended[1])) {
      switch (TYPE(tended[1])) {
	 case T_TVSUBS:
	    if (cvstr(&tended[2], sbuf1) == NULL)
               runerr(103, &tended[2]);
	    tended[3] = BLKLOC(tended[1])->tvsubs.ssvar;
	    if (cvstr(&tended[3], sbuf2) == NULL)
	       runerr(103, &tended[3]);
	    sneed(STRLEN(tended[3]) + STRLEN(tended[2]));
	    bp = BLKLOC(tended[1]);
	    l1 = bp->tvsubs.sspos - 1;
	    STRLEN(tended[4]) = bp->tvsubs.sslen;
	    STRLOC(tended[4]) = STRLOC(tended[3]) + l1;
	    l2 = l1 + STRLEN(tended[4]);
	    if (l2 > STRLEN(tended[3]))
	       runerr(205,NULL);
	    STRLOC(tended[5]) = alcstr(STRLOC(tended[3]), l1);
  	                        alcstr(STRLOC(tended[2]), STRLEN(tended[2]));
    	                        alcstr(STRLOC(tended[3])+l2, STRLEN(tended[3])-l2);
	    STRLEN(tended[5]) = STRLEN(tended[3]) - STRLEN(tended[4]) + STRLEN(tended[2]);
	    bp->tvsubs.sslen = STRLEN(tended[2]);
	    tended[1] = bp->tvsubs.ssvar;
	    tended[2] = tended[5]; /* tail recursion: */
	    goto assign;           /*   doasgn(bp->tvsubs.ssvar,tended[5]) */

	 case T_TVTBL:
	    hneed(sizeof(struct b_telem));
	    bp = BLKLOC(tended[1]);
	    tended[3] = bp->tvtbl.tvtref;
	    tp = BLKLOC(bp->tvtbl.tvtable);
	    l1 = hash(&tended[3]);
	    bp = BLKLOC(tp->buckets[l1]);
	    while (bp != NULL) {	/* make sure it's not there */
	       if (equiv(&bp->telem.tref, &tended[3])) {
		  bp->telem.tval = tended[2];        /* surprise! it's there! */
		  clrtend();
		  return;
	          }
	       bp = BLKLOC(bp->telem.blink);
	       }
            tp->cursize++;
	    BLKLOC(tp->buckets[l1]) =
	       alctelem(&tp->buckets[l1], &tended[3], &tended[2]);
	    tp->buckets[l1].type = D_TELEM;
	    clrtend();
	    return;

	 case T_TVPOS:
	    switch (cvint(&tended[2], &l3)) {
               case T_INTEGER:  break;
#ifndef BIT32
               case T_LONGINT:  clrtend(); fail();
#endif
               default:         runerr(101, &tended[2]);
               }
	    l1 = cvpos(l3, STRLEN(k_subject));
	    if (l1 <= 0) {
	       clrtend();
	       fail();
	       }
            k_pos = l1;
	    clrtend();
	    return;

	 case T_TVRAND:
	    switch (cvint(&tended[2], &l3)) {
               case T_INTEGER:
#ifndef BIT32
               case T_LONGINT:
#endif
				break;
               default:         runerr(101, &tended[2]);
               }
	    k_random = l3;
	    clrtend();
	    return;

	 case T_TVTRACE:
	    switch (cvint(&tended[2], &l3)) {
               case T_INTEGER:  k_trace = (int)l3; break;
#ifndef BIT32
               case T_LONGINT:  k_trace = -1; break;
#endif
               default:         runerr(101, &tended[2]);
               }
	    clrtend();
	    return;

	 default:
            syserr("doasgn: illegal trapped variable");
	 }
      }

   if (VARLOC(tended[1]) == &k_subject) {
      switch (cvstr(&tended[2], sbuf1)) {
	 case NULL:
	    runerr(103, &tended[2]);
	 case 1:
	    sneed(STRLEN(tended[2]));
	    STRLOC(tended[2]) = alcstr(STRLOC(tended[2]), STRLEN(tended[2]));
	 case 2:
	    k_subject = tended[2];
      	    k_pos = 1;
	 }
      }
   else
      *VARLOC(tended[1]) = tended[2];
   clrtend();
   return;
   }

clrtend()
   {
   register struct descrip *p;
   extern struct descrip tended[];

   for (p = &tended[1]; p <= &tended[5]; p++)
      *p = nulldesc;
   }
