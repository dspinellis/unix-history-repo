#include "../h/rt.h"
#ifdef VAX
/*
 * ^x - return an entry block for co-expression x from the refresh block.
 */

refresh(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int *ap, *sp, *tp;
   struct descrip *dp, *dsp;
   register struct b_estack *ep;
   register struct b_eblock *hp;
   int na, nl, *r5, *newap;
   extern struct b_estack *alcestk();
   extern interp();

   SetBound;
   DeRef(arg1)
   if (QUAL(arg1) || TYPE(arg1) != T_ESTACK)
      runerr(118, &arg1);

   /*
    * Get a new co-expression stack and initialize activator, result
    *  count, stack base, and fresh block location.
    */
   esneed();
   ep = alcestk();
   ep->activator = nulldesc;
   ep->nresults = 0;
   ep->freshblk = BLKLOC(arg1)->estack.freshblk;
   ep->sbase = sp = (int *)ep;
   /*
    * Get number of arguments, number of locals, and a pointer to
    *  arg0 in the refresh block's argument list.
    */
   hp = (struct b_eblock *) BLKLOC(ep->freshblk);
   na = hp->numargs;
   nl = hp->numlocals;
   ap = (int *) hp->elems;

   /*
    * Copy arguments into new stack and refresh block.
    */
   dsp = (struct descrip *) sp;
   dp = (struct descrip *) ap;
   *--dsp = *dp++;
   while (na-- > 0) {
      *--dsp = *dp++;
      }
   sp = (int *) dsp;
   ap = (int *) dp;

   /*
    * Set up original procedure frame in new stack.
    */
   *--sp = hp->numargs;			/* nargs */
   *--sp = (hp->numargs*2) + 1;		/* nwords */
   newap = sp;				/* save address of nwords */
   *--sp = (int) interp;		/* return pc */
   *--sp = 0;				/* saved r5 (frame pointer) */
   *--sp = 0;				/* saved ap */
   *--sp = 0;				/* psw/reg. mask */
   *--sp = 0;				/* condition handler */
   r5 = sp;				/* (save its address) */
   *--sp = line;			/* saved line number */
   *--sp = (int) file;			/* saved file name */

   /*
    * Copy local variables into new stack.
    */
   dsp = (struct descrip *) sp;
   dp = (struct descrip *) ap;
   while (nl-- > 0) {
      *--dsp = *dp++;
      }
   sp = (int *) dsp;
   ap = (int *) dp;

   /*
    * Set up dummy call to coact.
    */
   *--sp = nulldesc.type;		/* place for result */
   *--sp = INTVAL(nulldesc);
   *--sp = nulldesc.type;		/* place for activate coexpr */
   *--sp = INTVAL(nulldesc);
   *--sp = 1;				/* nargs */
   *--sp = 3;				/* nwords */
   tp = sp;				/* save pointer to start of arg
                                            list in this frame */
   *--sp = (int) hp->ep;		/* saved r9 (coexpr entry point) */
   *--sp = (int) interp;		/* return pc (entry point) */
   *--sp = (int) r5;			/* saved r5 */
   *--sp = (int) newap;			/* saved ap */
   *--sp = 0x02000000;			/* psw/reg mask with bit set to
                                            restore r9, the ipc */
   *--sp = 0;
   ep->boundary = sp;			/* (initial boundary) */
   *--sp = line;			/* saved line number */
   *--sp = (int) file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   ep->ap = tp;
   
   /*
    * Return the new co-expression.
    */
   arg0.type = D_ESTACK;
   BLKLOC(arg0) = (union block *) ep;
   ClearBound;
   }

Opblock(refresh,1,"^")
#endif VAX

#ifdef PORT
refresh()
{
   syserr("Attempt to refresh a co-expression");
}
#endif PORT
#ifdef PDP11
/*
 * ^x - return an entry block for co-expression x from the refresh block.
 * NOTE:  this code is highly dependent on stack frame layout.
 */

refresh(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int *ap, *sp;
   register struct b_estack *ep;
   register struct b_eblock *hp;
   int na, nl, *r5;
   extern struct b_estack *alcestk();
   extern interp();

   DeRef(arg1)
   if (QUAL(arg1) || TYPE(arg1) != T_ESTACK)
      runerr(118, &arg1);

   esneed();                            /* check for room in stack space */
   ep = alcestk();                      /* allocate expression stack */
   ep->activator = nulldesc;
   ep->nresults = 0;
   ep->freshblk = BLKLOC(arg1)->estack.freshblk;

   ep->sbase = sp = (int *)ep;		/* initialize new stack pointer */
   hp = BLKLOC(ep->freshblk);
   na = hp->numargs;		        /* get value of nargs */
   nl = hp->numlocals;
   ap = hp->elems;               	/* find arg0 of caller */

   /* copy arguments into new stack and refresh block */
   *--(struct descrip *)sp = *(struct descrip *)ap;   /* copy arg0 */
   while (na-- > 0)
      *--(struct descrip *)sp = *++(struct descrip *)ap;

   /* set up original procedure frame in new stack */
   *--sp = hp->numargs;			/* copy nargs */
   *--sp = 0;				/* return pc */
   *--sp = 0;				/* saved r5 */
   r5 = sp;				/*   (save its address) */
   *--sp = 0;				/* saved r4 */
   *--sp = 0;				/* saved r3 */
   *--sp = 0;				/* saved r2 */
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */

   /* copy local variables into new stack */
   while (nl-- > 0)
      *--(struct descrip *)sp = *++(struct descrip *)ap;

   /* set up dummy call to activate */
   *--(struct descrip *)sp = nulldesc;  /* place for result */
   *--(struct descrip *)sp = nulldesc;  /* place for activate coexpr */
   /* these values are the initial register state for the coexpression */
   *--sp = 1;				/* nargs */
   *--sp = interp;           		/* return pc (entry point) */
   *--sp = r5;				/* saved r5 */
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = 0;				/* saved r4 */
   *--sp = 0;				/* saved r3 */
   *--sp = hp->ep;			/* saved r2 */
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   arg0.type = D_ESTACK;
   BLKLOC(arg0) = ep;
   }
Opblock(refresh,1,"^")
#endif PDP11
