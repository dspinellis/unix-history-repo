/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)memcmp.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* int memcmp(s1, s2, n) */

#include "DEFS.h"

ENTRY(memcmp, 0)
	movl	4(ap),r1	/* r1 = s1 */
	movq	8(ap),r3	/* r3 = s2; r4 = n */
	movzwl	$65535,r5
0:
	cmpl	r4,r5
	jgtru	3f		/* handle stupid cmpc3 limitation */
	cmpc3	r4,(r1),(r3)	/* compare */
	beql	2f		/* done if same (r0 = 0) */
1:
	movzbl	(r1),r0
	movzbl	(r3),r2
	subl2	r2,r0		/* return *s1 - *s2; s1,s2 unsigned chars */
2:
	ret
3:
	subl2	r5,r4		/* do 64K; adjust count */
	cmpc3	r5,(r1),(r3)
	jeql	0b		/* loop if same */
	jbr	1b
