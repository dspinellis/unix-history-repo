/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigsuspend.s	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.even
err:
	jmp	cerror

ENTRY(sigsuspend)
	movl	sp@(4),a0
	movl	a0@,sp@(4)		/* indirect to mask arg */
	movl	#SYS_sigsuspend,d0
	trap	#0
	jcs	err
	clrl	d0			/* shouldn't happen */
	rts
