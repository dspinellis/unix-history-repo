#include "../h/rt.h"
#include "../h/record.h"

/*
 * !x - generate successive values from object x.
 * Generator.
 */

bang(nargs, arg1v, arg1, arg0)
int nargs;
struct descrip arg1v, arg1, arg0;
   {
   register int i, j, slen;
   register union block *bp, *ep;
   register struct descrip *dp;
   int ub, typ1;
   char sbuf[MAXSTRING];
   FILE *fd;
   extern char *alcstr();

   SetBound;
   arg1v = arg1;

   if ((typ1 = cvstr(&arg1, sbuf)) != NULL) {
      i = 1;
      while (i <= STRLEN(arg1)) {
         if (typ1 == 1) {
            sneed(1);
            STRLEN(arg0) = 1;
            STRLOC(arg0) = alcstr(STRLOC(arg1)+i-1, 1);
            suspend();
            }
         else {
            hneed(sizeof(struct b_tvsubs));
            mksubs(&arg1v, &arg1, i, 1, &arg0);
            suspend();
            arg1 = arg1v;
            deref(&arg1);
            if (!QUAL(arg1))
               runerr(103, &arg1);
            }
         i++;
         }
      }
   else {
      deref(&arg1);
      switch (TYPE(arg1)) {
         case T_LIST:
            bp = BLKLOC(arg1);
            for (arg1 = bp->list.listhead; arg1.type == D_LISTB;
	         arg1 = BLKLOC(arg1)->listb.listnext) {
               bp = BLKLOC(arg1);
               for (i = 0; i < bp->listb.nused; i++) {
		  j = bp->listb.first + i;
		  if (j >= bp->listb.nelem)
		     j -= bp->listb.nelem;
		  dp = &bp->listb.lelem[j];
                  arg0.type = D_VAR + ((int *)dp - (int *)bp);
                  BLKLOC(arg0) = dp;
                  suspend();
                  bp = BLKLOC(arg1);   /* bp is untended, must reset */
                  }
               }
            break;

         case T_FILE:
            fd = BLKLOC(arg1)->file.fd;
            if ((BLKLOC(arg1)->file.status & FS_READ) == 0)
               runerr(212, &arg1);
            while ((slen = getstr(sbuf,MAXSTRING,fd)) >= 0) {
               sneed(slen);
               STRLEN(arg0) = slen;
               STRLOC(arg0) = alcstr(sbuf,slen);
               suspend();
               }
            break;

         case T_TABLE:
            for (i = 0; i < NBUCKETS; i++) {
               bp = BLKLOC(arg1);
               for (arg1v = bp->table.buckets[i]; arg1v.type == D_TELEM;
	            arg1v = BLKLOC(arg1v)->telem.blink) {
	          ep = BLKLOC(arg1v);
                  dp = &ep->telem.tval;
                  arg0.type = D_VAR + ((int *)dp - (int *)bp);
                  BLKLOC(arg0) = dp;
                  suspend();
                  bp = BLKLOC(arg1);   /* bp is untended, must reset */
                  }
               }
            break;

	 case T_RECORD:
	    bp = BLKLOC(arg1);
	    j = bp->record.recptr->nfields;
	    for (i = 0; i < j; i++) {
	       dp = &bp->record.fields[i];
	       arg0.type = D_VAR + ((int *)dp - (int *)bp);
	       BLKLOC(arg0) = dp;
	       suspend();
               bp = BLKLOC(arg1);   /* bp is untended, must reset */
	       }
	    break;

         default:
            runerr(116, &arg1);
         }
      }

   fail();
   }
struct b_iproc Bbang = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(bang),
   2,
   -1,
   -1,
   0,
   {1, "!"}
   };
