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
	.asciz "@(#)memset.s	5.1 (Berkeley) 5/15/90"
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
