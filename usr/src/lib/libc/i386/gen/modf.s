/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sean Eric Fagan
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
.asciz  "@(#)modf.c	5.1 (Berkeley) 4/23/90"
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
/APP
	fnstcw -12(%ebp)
/NO_APP
	movw -12(%ebp),%dx
	orw $3072,%dx
	movw %dx,-16(%ebp)
/APP
	fldcw -16(%ebp)
/NO_APP
	fldl 8(%ebp)
/APP
	frndint
/NO_APP
	fstpl -8(%ebp)
/APP
	fldcw -12(%ebp)
/NO_APP
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
