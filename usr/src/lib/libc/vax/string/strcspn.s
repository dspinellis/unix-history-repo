/*-
 * Copyright (c) 1990 The Regents of the University of California.
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
	.asciz "@(#)strcspn.s	5.1 (Berkeley) 5/15/90"
#endif /* LIBC_SCCS and not lint */

/*
 * Span the complement of string s2 (skip characters that are not in s2).
 * Return the number of characters in s1 that were skipped.
 *
 * size_t
 * strcspn(s1, s2)
 *	const char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strcspn, 0)
	subl2	$32,sp		/* make 256 bit table */
	movc5	$0,(sp),$0,$32,(sp)
	movq	4(ap),r1	/* r1 = s1, r2 = s2 */

	/* turn on bit for each character in s2, including '\0' */
1:
	movzbl	(r2)+,r0
	bbss	r0,(sp),1b
	bneq	1b
	movl	r1,r0		/* r0 = s (current pos in s1) */

	/* look for a character that is in s2 */
2:
	movzbl	(r0)+,r2	/* c = *s++ */
	bbc	r2,(sp),2b	/* loop until c is in table */
	decl	r0		/* s-- */
	subl2	r1,r0		/* r0 = s - s1 = count */
	ret
