/*
 * Copyright (c) 1988 Regents of the University of California.
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
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)bcmp.s	1.4 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

/* bcmp(s1, s2, n) */
/* compare exactly 'n' */
#include "DEFS.h"

ENTRY(bcmp, 0)
	movl	4(fp),r0
	movl	8(fp),r1
	mnegl	12(fp),r2
	jeql	3f
1:
	cmpb	(r0),(r1)
	jneq	2f
	incl	r0
	incl	r1
	aoblss	$0,r2,1b
3:
	clrl	r0
	ret
2:
	movl	$1,r0
	ret
