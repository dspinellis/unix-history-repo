#include "../h/rt.h"
#ifdef VAX
/*
 * create - return an entry block for a co-expression.
 */

create(nargs, arg1)
int nargs;
struct descrip arg1;
   {
   register int *ap, *sp, *tp;
   register struct descrip *dp;
   register struct b_estack *ep;
   register struct b_eblock *hp;
   int na, nl, *r5, i, j, *newap;
   extern interp();
   extern struct b_estack *alcestk();
   extern struct b_eblock *alceblk();

   SetBound;
   if (QUAL(arg1) || TYPE(arg1) != T_INTEGER)
       syserr("create: illegal entry point");

   /*
    * Get a new co-expression stack and initialize activator, result
    *  count, and stack base.
    */
   esneed();
   ep = alcestk();
   ep->activator = nulldesc;
   ep->nresults = 0;
   ep->sbase = sp = (int *)ep;
   /*
    * Move hardware ap of calling function into the variable ap.
    */
   asm(" addl3 8(fp),$4,r11");
   /*
    * Calculate number of arguments and number of local variables.
    */
   na = *ap;
   tp = ap + 1 + (2 * na);
   nl = ((struct descrip *)tp)->value.bptr->proc.ndynam;

   /*
    * Get a new co-expression heap block.  Note that the "+ 1" in
    *  na + nl + 1 is for arg0, the calling procedure.
    */
   hneed(sizeof(struct b_eblock) + (na + nl + 1) * sizeof(struct descrip));
   hp = alceblk(INTVAL(arg1),na,nl);
   ep->freshblk.type = D_EBLOCK;
   BLKLOC(ep->freshblk) = (union block *) hp;

   /*
    * Copy arguments into refresh block.  dp starts at arg0 and works down.
    */
   dp = (struct descrip *) ((int *)(ap + 1 + (2 * na)));
   i = 0;
   hp->elems[0] = *dp--;
   j = na;
   while (j-- > 0) {
      hp->elems[++i] = *dp--;
      }

   /*
    * Copy arguments into new stack.  This is more painful than copying
    *  into the refresh block because the arguments are copied a word
    *  at a time.  tp starts at the high word of arg0 and goes down.
    */
   tp = ap + 2 + (2 * na);
   *--sp = *tp--;
   *--sp = *tp--;
   j = na;
   while (j-- > 0) {
      *--sp = *tp--;
      *--sp = *tp--;
      }

   /*
    * Set up original procedure frame in new stack.
    */
   *--sp = *tp--;			/* copy nargs */
   *--sp = *tp;				/* copy nwords (argc) */
   newap = sp;				/*  and save a pointer to it. */
   *--sp = 0;				/* return pc */
   *--sp = 0;				/* saved r5 (frame pointer) */
   *--sp = 0;				/* saved ap */
   *--sp = 0;				/* psw/reg. mask */
   *--sp = 0;				/* condition handler */
   r5 = sp;				/*  and save a pointer to it */
   *--sp = line;			/* saved line number */
   *--sp = (int) file;			/* saved file name */

   /*
    * Copy local variables into new stack.  The asm sets tp to the
    *  address of the start of local region in caller's procedure frame.
    */
   asm(" addl3 12(fp),$-12,r9");
   j = nl;
   while (j-- > 0) {
      *--sp = *tp--;
      *--sp = *tp--;
     }
   /*
    * Copy local variables into the refresh block.  The asm sets dp to
    *  the address of the first local.
    */
   asm(" addl3 12(fp),$-16,r8");
   j = nl;
   while (j-- > 0)
      hp->elems[++i] = *dp--;

   /*
    * Set up dummy call to coact.
    */
   *--sp = nulldesc.type;		/* place for result */
   *--sp = INTVAL(nulldesc);
   *--sp = nulldesc.type;		/* place for activate coexpr */
   *--sp = INTVAL(nulldesc);
   *--sp = 1;				/* nargs */
   *--sp = 3;				/* 3 longwords in nargs */
   tp = sp;				/* save pointer to start of arg
					    list in this frame */
   *--sp = INTVAL(arg1);		/* saved r9 (coexpr entry point) */
   *--sp = (int) interp;			/* return pc */
   *--sp = (int) r5;				/* saved r5 */
   *--sp = (int) newap;			/* saved ap */
   *--sp = 0x02000000;			/* psw/reg mask with bit set to
					    restore r9, the ipc */
   *--sp = 0;				/* condition handler */
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = line;			/* saved line number */
   *--sp = (int) file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   ep->ap = tp;				/* initial arg. pointer */

   /*
    * Return the new co-expression.
    */
   arg1.type = D_ESTACK;
   BLKLOC(arg1) = (union block *) ep;
   ClearBound;
   }
#endif VAX
#ifdef PORT
create()
{
   syserr("Attempt to create a co-expression");
}
#endif PORT

#ifdef PDP11
/*
 * create - return an entry block for a co-expression
 *
 * NOTE:  this code is highly dependent on stack frame layout.
 */

create(nargs, arg1)
int nargs;
struct descrip arg1;
   {
   register int *ap, *sp;
   register struct b_estack *ep;
   register struct b_eblock *hp;
   int na, nl, *r5, i;
   extern interp();
   extern struct b_estack *alcestk();
   extern struct b_eblock *alceblk();

   if (QUAL(arg1) || TYPE(arg1) != T_INTEGER)
       syserr("create: illegal entry point");

   esneed();                            /* check for room in stack space */
   ep = alcestk();                      /* allocate expression stack */
   ep->activator = nulldesc;
   ep->nresults = 0;

   ep->sbase = sp = (int *)ep;		/* initialize new stack pointer */
   ap = (int *)*(&nargs-2) + 2;		/* find nargs of caller */
   na = *ap;			        /* get value of nargs */
   ap += 1 + 2 * na;			/* find arg0 of caller */
   nl = ((struct descrip *)ap)->value.bptr->proc.ndynam; /* get # locals */

   hneed(sizeof(struct b_eblock) + (na + nl + 1) * sizeof(struct descrip));
   hp = alceblk(INTVAL(arg1),na,nl);    /* allocate refresh block */
   ep->freshblk.type = D_EBLOCK;
   BLKLOC(ep->freshblk) = (union block *) hp;

   /* copy arguments into new stack and refresh block */
   i = 0;
   hp->elems[i] = *--(struct descrip *)sp = *(struct descrip *)ap;
   while (na-- > 0)
      hp->elems[++i] = *--(struct descrip *)sp = *--(struct descrip *)ap;

   /* set up original procedure frame in new stack */
   *--sp = *--ap;			/* copy nargs */
   *--sp = 0;				/* return pc */
   *--sp = 0;				/* saved r5 */
   r5 = sp;				/*   (save its address) */
   *--sp = 0;				/* saved r4 */
   *--sp = 0;				/* saved r3 */
   *--sp = 0;				/* saved r2 */
   *--sp = line;			/* saved line number */
   *--sp = (int) file;			/* saved file name */

   /* copy local variables into new stack */
   ap -= 7;
   while (nl-- > 0)
      hp->elems[++i] = *--(struct descrip *)sp = *--(struct descrip *)ap;

   /* set up dummy call to coact */
   *--(struct descrip *)sp = nulldesc;  /* place for result */
   *--(struct descrip *)sp = nulldesc;  /* place for activate coexpr */
   /* these values are the initial register state for the co-expression */
   *--sp = 1;				/* nargs */
   *--sp = (int) interp;			/* return pc */
   *--sp = (int) r5;				/* saved r5 */
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = 0;				/* saved r4 */
   *--sp = 0;				/* saved r3 */
   *--sp = INTVAL(arg1);		/* saved r2 (coexpr entry point) */
   *--sp = line;			/* saved line number */
   *--sp = (int) file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   arg1.type = D_ESTACK;
   BLKLOC(arg1) = (union block *) ep;
   }
#endif PDP11
