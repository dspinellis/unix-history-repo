/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)memset.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* void *memset(base, c, length) */

#include "DEFS.h"

ENTRY(memset, 0)
	movl	4(ap),r3
1:
	movzwl	$65535,r0
	movq	8(ap),r1
	cmpl	r2,r0
	jgtru	2f
	movc5	$0,(r3),r1,r2,(r3)
	ret
2:
	subl2	r0,12(ap)
	movc5	$0,(r3),r1,r0,(r3)
	jbr	1b
