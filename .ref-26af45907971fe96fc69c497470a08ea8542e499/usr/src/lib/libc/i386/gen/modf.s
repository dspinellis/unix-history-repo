/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sean Eric Fagan.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)modf.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * modf(value, iptr): return fractional part of value, and stores the
 * integral part into iptr (a pointer to double).
 *
 * Written by Sean Eric Fagan (sef@kithrup.COM)
 * Sun Mar 11 20:27:30 PST 1990
 */

/* With CHOP mode on, frndint behaves as TRUNC does.  Useful. */
.text
.globl _modf
_modf:
	pushl %ebp
	movl %esp,%ebp
	subl $16,%esp
	fnstcw -12(%ebp)
	movw -12(%ebp),%dx
	orw $3072,%dx
	movw %dx,-16(%ebp)
	fldcw -16(%ebp)
	fldl 8(%ebp)
	frndint
	fstpl -8(%ebp)
	fldcw -12(%ebp)
	movl 16(%ebp),%eax
	movl -8(%ebp),%edx
	movl -4(%ebp),%ecx
	movl %edx,(%eax)
	movl %ecx,4(%eax)
	fldl 8(%ebp)
	fsubl -8(%ebp)
	jmp L1
L1:
	leave
	ret
