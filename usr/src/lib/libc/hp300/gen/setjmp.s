/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)setjmp.s	5.1 (Berkeley) 5/12/90"
#endif /* LIBC_SCCS and not lint */

/*
 * C library -- setjmp, longjmp
 *
 *	longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	setjmp(a)
 * by restoring registers from the stack,
 * and a struct sigcontext, see <signal.h>
 */

#include "DEFS.h"

ENTRY(setjmp)
	subql	#8,sp		/* space for sigstack args/rvals */
	clrl	sp@		/* don't change it... */
	movl	sp,sp@(4)	/* ...but return the current val */
	jsr	_sigstack	/* note: onstack returned in sp@(4) */
	clrl	sp@		/* don't change mask, just return */
	jsr	_sigblock	/*   old value */
	movl	sp@(4),d1	/* old onstack value */
	addql	#8,sp
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

ENTRY(longjmp)
	movl	sp@(4),a0	/* save area pointer */
	tstl	a0@(8)		/* ensure non-zero SP */
	jeq	botch		/* oops! */
	movl	sp@(8),d0	/* grab return value */
	jne	ok		/* non-zero ok */
	moveq	#1,d0		/* else make non-zero */
ok:
	moveml	a0@(28),#0x3CFC	/* restore non-scratch regs */
	movl	a0,sp@-		/* let sigreturn */
	jsr	_sigreturn	/*   finish for us */

botch:
	jsr	_longjmperror
	stop	#0
