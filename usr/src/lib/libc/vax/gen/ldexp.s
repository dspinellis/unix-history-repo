/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
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
	.asciz "@(#)ldexp.s	5.6 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

/*
 * double ldexp (value, exp)
 * double value;
 * int exp;
 *
 * Ldexp returns value*2**exp, if that result is in range.
 * If underflow occurs, it returns zero.  If overflow occurs,
 * it returns a value of appropriate sign and largest
 * possible magnitude.  In case of either overflow or underflow,
 * errno is set to ERANGE.  Note that errno is not modified if
 * no error occurs.
 */

#include "DEFS.h"

/*
 * don't include errno.h, ANSI C says it defines errno.
 *
 * #include <errno.h>
 */
#define	ERANGE	34

	.globl	_errno

ENTRY(ldexp, 0)
	movd	4(ap),r0	/* fetch "value" */
	extzv	$7,$8,r0,r2	/* r2 := biased exponent */
	jeql	1f		/* if zero, done */

	addl2	12(ap),r2	/* r2 := new biased exponent */
	jleq	2f		/* if <= 0, underflow */
	cmpl	r2,$256		/* otherwise check if too big */
	jgeq	3f		/* jump if overflow */
	insv	r2,$7,$8,r0	/* put exponent back in result */
1:
	ret
2:
	clrd	r0
	jbr	1f
3:
	movd	huge,r0		/* largest possible floating magnitude */
	jbc	$15,4(ap),1f	/* jump if argument was positive */
	mnegd	r0,r0		/* if arg < 0, make result negative */
1:
	movl	$ERANGE,_errno
	ret

	.data
huge:	.word	0x7fff		/* the largest number that can */
	.word	0xffff		/*   be represented in a long floating */
	.word	0xffff		/*   number.  This is given in hex in order */
	.word	0xffff		/*   to avoid floating conversions */
