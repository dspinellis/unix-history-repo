/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)setjmp.s	1.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * C library -- setjmp, longjmp
 *
 *	longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	setjmp(a)
 * by restoring registers from the stack,
 * previous signal mask, and doing a return.
 */

#include "DEFS.h"

ENTRY(setjmp, R6)
	movl	4(fp),r6		# construct sigcontext
	movab	-8(sp),sp		# space for current struct sigstack
	pushal	(sp)			# get current values
	pushl	$0			# no new values
	callf	$16,_sigstack		# pop args plus signal stack value
	movl	(sp)+,(r6)		# save onsigstack status of caller
	pushl	$0
	callf	$8,_sigblock		# get signal mask
	movl	r0,4(r6)		# save signal mask of caller
	addl3	$8,fp,8(r6)		# save stack pointer of caller
	movl	(fp),12(r6)		# save frame pointer of caller
	movl	-8(fp),20(r6)		# save pc of caller
	movpsl	24(r6)			# save psl of caller
	clrl	r0
	ret

ENTRY(longjmp, 0)
	movl	8(fp),r0		# return(v)
	movl	4(fp),r1		# fetch buffer
	tstl	12(r1)
	beql	botch
loop:
	cmpl	12(r1),(fp)
	beql	done
	blssu	botch
	movl	$loop,-8(fp)
	ret				# pop another frame

done:
	cmpb	*-8(fp),reiins		# returning to an "rei"?
	bneq	1f
	movab	3f,-8(fp)		# do return w/ psl-pc pop
	brw	2f
1:
	movab	4f,-8(fp)		# do standard return
2:
	ret				# unwind stack before signals enabled
3:
	addl2	$8,sp			# compensate for PSL-PC push
4:
	pushl	r1			# pointer to sigcontext
	callf	$4,_sigreturn		# restore previous context
					# we should never return

botch:
	callf	$4,_longjmperror
	halt

	.data
reiins:	rei
