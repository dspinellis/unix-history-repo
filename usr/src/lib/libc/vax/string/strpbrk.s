/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strpbrk.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Find in s1 the first occurrence of any character from s2.
 * If there are none, return NULL.
 *
 * char *
 * strpbrk(s1, s2)
 *	const char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strpbrk, 0)
	subl2	$32,sp		/* make 256 bit table */
	movc5	$0,(sp),$0,$32,(sp)
	movq	4(ap),r0	/* r0 = s1, r1 = s2 */

	/* turn on bit for each character in s2, including '\0' */
1:
	movzbl	(r1)+,r2
	bbss	r2,(sp),1b
	bneq	1b

	/* look for a character that is in s2 */
2:
	movzbl	(r0)+,r2	/* c = *s++ */
	bbc	r2,(sp),2b	/* loop until c is in table */
	beql	3f		/* if c==0, go return NULL */
	decl	r0		/* s-- */
	ret
3:
	clrl	r0
	ret
