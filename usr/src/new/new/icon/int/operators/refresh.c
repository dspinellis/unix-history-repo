#include "../h/rt.h"
#ifdef VAX
/*
 * ^x - return an entry block for co-expression x from the refresh block.
 * NOTE:  this code is highly dependent on stack frame layout.
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
#ifdef INT
   extern interp();
#endif INT

   SetBound;
   deref(&arg1);
   if (TYPE(arg1) != T_ESTACK)
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
   dsp = sp;
   dp = ap;
   *--dsp = *dp++;
   while (na-- > 0) {
      *--dsp = *dp++;
      }
   sp = dsp;
   ap = dp;

   /* set up original procedure frame in new stack */
   *--sp = hp->numargs;			/* copy nargs */
   *--sp = (hp->numargs*2) + 1;		/*+ invent a nwords value ??-whm */
   newap = sp;
#ifdef INT
   *--sp = interp;			/* return pc */
#endif INT
#ifdef CMP
   *--sp = 0;				/* return pc */
#endif CMP
   *--sp = 0;				/* saved r5 (frame pointer) */
   *--sp = 0;				/* saved ap */
   *--sp = 0;				/* psw/reg. mask */
   *--sp = 0;				/* condition handler */
   r5 = sp;				/*   (save its address) */
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */

   /* copy local variables into new stack */
   dsp = sp;
   dp = ap;
   while (nl-- > 0) {
      *--dsp = *dp++;
      }
   sp = dsp;
   ap = dp;

   /* set up dummy call to activate */
   *--sp = nulldesc.type;		/* place for result */
   *--sp = INTVAL(nulldesc);
   *--sp = nulldesc.type;		/* place for activate coexpr */
   *--sp = INTVAL(nulldesc);
   /* these values are the initial register state for the coexpression */
   *--sp = 1;				/* nargs */
   *--sp = 3;				/* nwords */
   tp = sp;				/* save pointer to start of arg
					    list in this frame */
#ifdef INT
   *--sp = hp->ep;		        /* saved r9 (coexpr entry point) */
   *--sp = interp;			/* return pc (entry point) */
#endif INT
#ifdef CMP
   *--sp = 0;			        /* saved r9 (coexpr entry point) */
   *--sp = hp->ep;			/* return pc (entry point) */
#endif CMP
   *--sp = r5;				/* saved r5 */
   *--sp = newap;			/* saved ap */
   *--sp = 0x02000000;			/* psw/reg mask with bit set to
   					    restore r9, the ipc */
   *--sp = 0;
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   ep->ap = tp;
   arg0.type = D_ESTACK;
   BLKLOC(arg0) = ep;
   ClearBound;
   }
struct b_iproc Brefresh = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(refresh),
   1,
   -1,
   0,
   0,
   {1, "^"}
   };
#endif VAX
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
#ifdef INT
   extern interp();
#endif INT

   deref(&arg1);
   if (TYPE(arg1) != T_ESTACK)
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
#ifdef INT
   *--sp = interp;           		/* return pc (entry point) */
#endif INT
#ifdef CMP
   *--sp = hp->ep;           		/* return pc (entry point) */
#endif CMP
   *--sp = r5;				/* saved r5 */
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = 0;				/* saved r4 */
   *--sp = 0;				/* saved r3 */
#ifdef INT
   *--sp = hp->ep;			/* saved r2 */
#endif INT
#ifdef CMP
   *--sp = 0;				/* saved r2 */
#endif CMP
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   arg0.type = D_ESTACK;
   BLKLOC(arg0) = ep;
   }
struct b_iproc Brefresh = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(refresh),
   1,
   -1,
   0,
   0,
   {1, "^"}
   };
#endif PDP11
