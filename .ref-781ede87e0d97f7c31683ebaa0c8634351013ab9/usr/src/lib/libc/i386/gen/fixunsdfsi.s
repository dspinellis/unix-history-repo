/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fixunsdfsi.s	5.1	%G%"
#endif /* LIBC_SCCS and not lint */

	.globl ___fixunsdfsi
___fixunsdfsi:
	fldl	4(%esp)		/* argument double to accum stack */
	frndint			/* create integer */
	fcoml	fbiggestsigned	/* bigger than biggest signed? */
	fstsw	%ax
	sahf
	jnb	1f
	
	fistpl	4(%esp)
	movl	4(%esp),%eax
	ret

1:	fsubl	fbiggestsigned	/* reduce for proper conversion */
	fistpl	4(%esp)		/* convert */
	movl	4(%esp),%eax
	orl	$0x80000000,%eax	/* restore bias */
	ret

fbiggestsigned:	.double	0r2147483648.0
