#include "../h/rt.h"
#ifdef VAX
/*
 * create - return an entry block for a co-expression
 *
 * NOTE:  this code is highly dependent on stack frame layout.
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
#ifdef INT
   extern interp();
#endif INT
   extern struct b_estack *alcestk();
   extern struct b_eblock *alceblk();

   SetBound;
   if (TYPE(arg1) != T_INTEGER)
       syserr("create: illegal entry point");

   esneed();                            /* check for room in stack space */
   ep = alcestk();                      /* allocate expression stack */
   ep->activator = nulldesc;
   ep->nresults = 0;

   ep->sbase = sp = (int *)ep;		/* initialize new stack pointer */
/*****
 *****  Note that on this next trick here, we have fuggered out whar
 *****   ap is going to be, namely in r11
 *****/
   asm(" addl3 8(fp),$4,r11");
   na = *ap;			        /* get value of nargs */
   tp = ap + 1 + (2 * na);
   nl = ((struct descrip *)tp)->value.bptr->proc.ndynam; /* get # locals */

   /*printf("Creating block with %d locals and %d arguments\n",nl,na);*/
   hneed(sizeof(struct b_eblock) + (na + nl + 1) * sizeof(struct descrip));
   hp = alceblk(INTVAL(arg1),na,nl);    /* allocate refresh block */
   ep->freshblk.type = D_EBLOCK;
   BLKLOC(ep->freshblk) = hp;

   /* copy arguments into refresh block */
   dp = (int *)(ap + 1 + (2 * na)); /* point at first word of arg0 */
   /*printf("copying arg0 from %x\n",dp);*/
   i = 0;
   hp->elems[0] = *dp--; /* always copy arg0 */
   j = na;
   while (j-- > 0) {
        /*printf("copying arg%d from %x\n",i,dp);*/
   	hp->elems[++i] = *dp--;
	}
	
   /* copy arguments into new stack */
   tp = ap + 2 + (2 * na);
   *--sp = *tp--;	/* copy arg0 */
   *--sp = *tp--;
   j = na;
   while (j-- > 0) {
   	*--sp = *tp--;
	*--sp = *tp--;
	}
   	
   /* set up original procedure frame in new stack */
   *--sp = *tp--;			/* copy nargs */
   *--sp = *tp;				/* copy nwords (argc) */
   newap = sp;
   *--sp = 0;				/* return pc */
   *--sp = 0;				/* saved r5 (frame pointer) */
   *--sp = 0;				/* saved ap */
   *--sp = 0;				/* psw/reg. mask */
   *--sp = 0;				/* condition handler */
   r5 = sp;				/*   (save its address) */
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */

   /* copy local variables into new stack */
/****
 **** Once again we have figured out that "tp" is in r9
 ****  This section needs cleanup particularly bad -- whm
 ****/
   asm(" addl3 12(fp),$-12,r9"); /* tp = addr of start of local region in
   				     caller's procedure frame */
   j = nl;
   while (j-- > 0) {
      *--sp = *tp--;
      *--sp = *tp--;
     }
   asm(" addl3 12(fp),$-16,r8"); /* dp = addr of first local */
   j = nl;
   while (j-- > 0)
      hp->elems[++i] = *dp--;

   /* set up dummy call to coact */
   *--sp = nulldesc.type;		/* place for result */
   *--sp = INTVAL(nulldesc);
   *--sp = nulldesc.type;		/* place for activate coexpr */
   *--sp = INTVAL(nulldesc);
   /* these values are the initial register state for the coexpression */
   *--sp = 1;				/* nargs */
   *--sp = 3;				/* 3 longwords in imag. nargs */
   tp = sp;				/* save pointer to start of arg
   					    list in this frame */
#ifdef INT
   *--sp = INTVAL(arg1);		/* saved r9 (coexpr entry point) */
   *--sp = interp;			/* return pc */
#endif INT
#ifdef CMP
   *--sp = 0;				/* saved r9 (coexpr entry point) */
   *--sp = INTVAL(arg1);		/* return pc */
#endif CMP
   *--sp = r5;				/* saved r5 */
   *--sp = newap;			/* saved ap */
   *--sp = 0x02000000;			/* psw/reg mask with bit set to
   					    restore r9, the ipc */
   *--sp = 0;				/* condition handler */
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   ep->ap = tp;				/* initial arg. pointer */
   arg1.type = D_ESTACK;
   BLKLOC(arg1) = ep;
   ClearBound;
   }
#endif VAX

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
#ifdef INT
   extern interp();
#endif INT
   extern struct b_estack *alcestk();
   extern struct b_eblock *alceblk();

   if (TYPE(arg1) != T_INTEGER)
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
   BLKLOC(ep->freshblk) = hp;

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
   *--sp = file;			/* saved file name */

   /* copy local variables into new stack */
   ap -= 7;
   while (nl-- > 0)
      hp->elems[++i] = *--(struct descrip *)sp = *--(struct descrip *)ap;

   /* set up dummy call to coact */
   *--(struct descrip *)sp = nulldesc;  /* place for result */
   *--(struct descrip *)sp = nulldesc;  /* place for activate coexpr */
   /* these values are the initial register state for the coexpression */
   *--sp = 1;				/* nargs */
#ifdef INT
   *--sp = interp;			/* return pc */
#endif INT
#ifdef CMP
   *--sp = INTVAL(arg1);		/* return pc (entry point) */
#endif CMP
   *--sp = r5;				/* saved r5 */
   ep->boundary = sp;			/*   (initial boundary) */
   *--sp = 0;				/* saved r4 */
   *--sp = 0;				/* saved r3 */
#ifdef INT
   *--sp = INTVAL(arg1);		/* saved r2 (coexpr entry point) */
#endif INT
#ifdef CMP
   *--sp = 0;				/* saved r2 */
#endif CMP
   *--sp = line;			/* saved line number */
   *--sp = file;			/* saved file name */
   ep->sp = sp;				/* initial stack pointer */
   arg1.type = D_ESTACK;
   BLKLOC(arg1) = ep;
   }
#endif PDP11
