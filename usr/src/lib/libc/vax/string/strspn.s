/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strspn.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Span the string s2 (skip characters that are in s2).
 * Return the number of characters in s1 that were skipped.
 *
 * size_t
 * strspn(s1, s2)
 *	const char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strspn, 0)
	subl2	$32,sp		/* make 256 bit table */
	movc5	$0,(sp),$0,$32,(sp)
	movq	4(ap),r1	/* r1 = s1, r2 = s2 */

	/* turn on bit for each character in s2, including '\0' */
1:
	movzbl	(r2)+,r0
	bbss	r0,(sp),1b
	bneq	1b

	/* now clear bit for '\0' */
	/* (this is easier than avoiding setting it in the first place) */
	bicb2	$1,(sp)		/* stop at '\0' */
	movl	r1,r0		/* r0 = s (current pos in s1) */

	/* look for a character that is not in s2 */
2:
	movzbl	(r0)+,r2	/* c = *s++ */
	bbs	r2,(sp),2b	/* loop while c is in table */
	decl	r0		/* s-- */
	subl2	r1,r0		/* r0 = s - s1 = count */
	ret
