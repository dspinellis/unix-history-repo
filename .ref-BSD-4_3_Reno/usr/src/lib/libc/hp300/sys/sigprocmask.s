/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigprocmask.s	5.2 (Berkeley) 6/6/90"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.even
err:
	jmp	cerror

ENTRY(sigprocmask)
	tstl	sp@(8)			/* check new sigset pointer */
	jne	gotptr			/* if not null, indirect */
/*	movl	#0,sp@(8)		/* null mask pointer: block empty set */
	movl	#1,sp@(4)		/* SIG_BLOCK */
	jra	doit
gotptr:
	movl	sp@(8),a0
	movl	a0@,sp@(8)		/* indirect to new mask arg */
doit:
	movl	#SYS_sigprocmask,d0
	trap	#0
	jcs	err
	tstl	sp@(12)			/* test if old mask requested */
	jeq	out
	movl	sp@(12),a0
	movl	d0,a0@			/* store old mask */
out:
	clrl	d0
	rts
