#include "../h/rt.h"

deref(d)
struct descrip *d;
   {
   register int i, j;
   register union block *bp;
   struct descrip v, tbl, tref;
   char sbuf[MAXSTRING];
   extern char *alcstr();

   if (!QUAL(*d) && VAR(*d)) {
      if (TVAR(*d)) {			/* trapped variables */
	 switch (TYPE(*d)) {
	    case T_TVSUBS:
	       bp = TVARLOC(*d);
               v = bp->tvsubs.ssvar;
	       switch (cvstr(&v, sbuf)) {
                  case NULL:
		     runerr(103,&v);
                  case 1:
		     sneed(STRLEN(v));
                     STRLOC(v) = alcstr(STRLOC(v), STRLEN(v));
                     bp = TVARLOC(*d);          /* alcstr is unstable! */
		     /* fall through */
                  case 2:
                     if (bp->tvsubs.sspos + bp->tvsubs.sslen - 1 > STRLEN(v))
                        runerr(205,NULL);
		     STRLEN(*d) = bp->tvsubs.sslen;
	             STRLOC(*d) = STRLOC(v) + bp->tvsubs.sspos - 1;
	          }
               break;

	    case T_TVTBL:
               tbl = BLKLOC(*d)->tvtbl.tvtable;
               tref = BLKLOC(*d)->tvtbl.tvtref;
               *d = BLKLOC(tbl)->table.defvalue;
  	       bp = BLKLOC(BLKLOC(tbl)->table.buckets[hash(&tref)]);
	       while (bp != NULL) {	      /* try to find element */
	          if (equiv(&bp->telem.tref, &tref)) {
	             *d = bp->telem.tval;
	             break;
	             }
	          bp = BLKLOC(bp->telem.blink);
	          }
	       break;

	    case T_TVPOS:
	       d->type = D_INTEGER;
	       INTVAL(*d) = k_pos;
	       break;

	    case T_TVRAND:
               mkint(k_random, d);
	       break;

	    case T_TVTRACE:
	       d->type = D_INTEGER;
	       INTVAL(*d) = k_trace;
	       break;

	    default:
	       syserr("deref: illegal trapped variable");
	    }
         }
      else
         *d = *VARLOC(*d);
      }
   if (!QUAL(*d) && VAR(*d))
      syserr("deref: didn't get dereferenced");
   return (1);
   }
