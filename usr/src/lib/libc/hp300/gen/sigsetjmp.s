/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigsetjmp.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * C library -- sigsetjmp, siglongjmp
 *
 *	siglongjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	sigsetjmp(a, savesig)
 * by restoring registers from the stack,
 * and a struct sigcontext, see <signal.h>
 */

#define _JBLEN	17	/* XXX from <setjmp.h> */

#include "DEFS.h"

ENTRY(sigsetjmp)
	tstl	sp@(8)		/* save signal mask? */
	beq	_setjmp		/* do _setjmp */
	movl	#_JBLEN,d0	/* last entry in jmpbuf */
	asll	#2,d0		/* scale to long ptr */
	movl	sp@(4),a0	/* save area pointer */
	addl	d0,a0		/* &jmpbuf[_JBLEN] */
	movl	sp@(8),a0@	/* jmpbuf[_JBLEN] = savemask */
	bra	setjmp		/* do setjmp */

_setjmp:
	movl	sp@(4),a0	/* save area pointer */
	clrl	a0@+		/* no old onstack */
	clrl	a0@+		/* no old sigmask */
	movl	sp,a0@+		/* save old SP */
	movl	a6,a0@+		/* save old FP */
	clrl	a0@+		/* no old AP */
	movl	sp@,a0@+	/* save old PC */
	clrl	a0@+		/* clear PS */
	moveml	#0x3CFC,a0@	/* save other non-scratch regs */
	clrl	d0		/* return zero */
	rts

setjmp:
	subl	#12,sp		/* space for sigaltstack args/rvals */
	clrl	sp@		/* don't change it... */
	movl	sp,sp@(4)	/* ...but return the current val */
	jsr	_sigaltstack	/* note: onstack returned in sp@(8) */
	clrl	sp@		/* don't change mask, just return */
	jsr	_sigblock	/*   old value */
	movl	sp@(8),d1	/* old onstack value */
	andl	#1,d1		/* extract onstack flag */
	addl	#12,sp
	movl	sp@(4),a0	/* save area pointer */
	movl	d1,a0@+		/* save old onstack value */
	movl	d0,a0@+		/* save old signal mask */
	lea	sp@(4),a1	/* adjust saved SP since we won't rts */
	movl	a1,a0@+		/* save old SP */
	movl	a6,a0@+		/* save old FP */
	clrl	a0@+		/* no AP */
	movl	sp@,a0@+	/* save old PC */
	clrl	a0@+		/* clean PS */
	moveml	#0x3CFC,a0@	/* save remaining non-scratch regs */
	clrl	d0		/* return 0 */
	rts

ENTRY(siglongjmp)
	movl	#_JBLEN,d0	/* last entry in jmpbuf */
	asll	#2,d0		/* scale to long ptr */
	movl	sp@(4),a0	/* save area pointer */
	addl	d0,a0		/* &jmpbuf[_JBLEN] */
	tstl	a0@		/* if jmpbuf[_JBLEN] */
	bne	longjmp		/*	do longjmp */

_longjmp:
	movl	sp@(4),a0	/* save area pointer */
	addql	#8,a0		/* skip onstack/sigmask */
	tstl	a0@		/* ensure non-zero SP */
	jeq	botch		/* oops! */
	movl	sp@(8),d0	/* grab return value */
	jne	ok		/* non-zero ok */
	moveq	#1,d0		/* else make non-zero */
ok:
	movl	a0@+,sp		/* restore SP */
	movl	a0@+,a6		/* restore FP */
	addql	#4,a0		/* skip AP */
	movl	a0@+,sp@	/* restore PC */
	moveml	a0@(4),#0x3CFC	/* restore non-scratch regs */
	rts

longjmp:
	movl	sp@(4),a0	/* save area pointer */
	tstl	a0@(8)		/* ensure non-zero SP */
	jeq	botch		/* oops! */
	movl	sp@(8),d0	/* grab return value */
	jne	good		/* non-zero good */
	moveq	#1,d0		/* else make non-zero */
good:
	moveml	a0@(28),#0x3CFC	/* restore non-scratch regs */
	movl	a0,sp@-		/* let sigreturn */
	jsr	_sigreturn	/*   finish for us */

botch:
	jsr	_longjmperror
	stop	#0
