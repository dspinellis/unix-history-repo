/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strncmp.s	1.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Compare strings (at most n bytes):  s1>s2: >0  s1==s2: 0  s1<s2: <0
 * 
 * strncmp(s1, s2, n)
 * register char *s1, *s2;
 * register n;
 */
#include "DEFS.h"

ENTRY(strncmp, 0)
	movl	12(fp),r2
	tstl	r2		/* number of bytes to compare */
	jgtr	n_ok
	clrl	r0
	ret			/* for n <= 0 , s1 == s2 */
n_ok:
	movl	4(fp),r0
	movl	8(fp),r1
	cmps3
	jgtr	greater
	jlss	less
equal:	clrl	r0
	ret
less:	movl	$-1,r0
	ret
greater: movl	$1,r0
	ret
