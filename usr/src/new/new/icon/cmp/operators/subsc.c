#include "../h/rt.h"
#include "../h/record.h"

/*
 * x[y] - access y'th character or element of x.
 */

subsc(nargs, arg1v, arg2, arg1, arg0)
int nargs;
struct descrip arg1v, arg2, arg1, arg0;
   {
   register int i, j;
   register union block *bp;
   int typ1;
   long l1;
   struct descrip *dp;
   char sbuf[MAXSTRING];
   extern char *alcstr();
   extern struct b_tvtbl *alctvtbl();

   SetBound;
   arg1v = arg1;

   if ((typ1 = cvstr(&arg1, sbuf)) != NULL) {   /* subscripting a string */
      if (cvint(&arg2, &l1) == NULL)
         runerr(101, &arg2);
      i = cvpos(l1, STRLEN(arg1));
      if (i > STRLEN(arg1))             /* fail if off string */
         fail();
      if (typ1 == 1) {			/* if string was created, */
         sneed(1);			/*   just return a string */
         STRLEN(arg0) = 1;
         STRLOC(arg0) = alcstr(STRLOC(arg1)+i-1, 1);
         }
      else {				/* else make a substring tv */
         hneed(sizeof(struct b_tvsubs));
         mksubs(&arg1v, &arg1, i, 1, &arg0);
	 }
      ClearBound;
      return;
      }

   deref(&arg1);
   switch (TYPE(arg1)) {                /* subscripting an aggregate */
      case T_LIST:
         if (cvint(&arg2, &l1) == NULL)
            runerr(101, &arg2);
	 i = cvpos(l1, BLKLOC(arg1)->list.cursize);
         if (i > BLKLOC(arg1)->list.cursize)    /* insure legal subscript */
            fail();
	 bp = BLKLOC(BLKLOC(arg1)->list.listhead);	
	 j = 1;
	 while (i >= j + bp->listb.nused) {
	    j += bp->listb.nused;
            if (TYPE(bp->listb.listnext) != T_LISTB)
	       syserr("list reference out of bounds in subsc");
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
         deref(&arg2);
	 hneed(sizeof(struct b_tvtbl));
  	 bp = BLKLOC(BLKLOC(arg1)->table.buckets[hash(&arg2)]);
	 while (bp != NULL) {
	    if (equiv(&bp->telem.tref, &arg2)) {
	       dp = &bp->telem.tval;
	       arg0.type = D_VAR + ((int *)dp - (int *)bp);
	       BLKLOC(arg0) = dp;
               ClearBound;
	       return;
	       }
	    bp = BLKLOC(bp->telem.blink);
	    }
	 arg0.type = D_TVTBL;
         BLKLOC(arg0) = alctvtbl(&arg1, &arg2);
         ClearBound;
	 return;

      case T_RECORD:
         if (cvint(&arg2, &l1) == NULL)
            runerr(101, &arg2);
   	 bp = BLKLOC(arg1);
	 i = cvpos(l1, bp->record.recptr->nfields);
	 if (i > bp->record.recptr->nfields)
	    fail();
	 dp = &bp->record.fields[i-1];
  	 arg0.type = D_VAR + ((int *)dp - (int *)bp);
         BLKLOC(arg0) = dp;
         ClearBound;
	 return;

      default:
	 runerr(114, &arg1);
      }	
   ClearBound;
   }
struct b_iproc Bsubsc = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(subsc),
   3,
   -1,
   -2,
   0,
   {2, "[]"}
   };
