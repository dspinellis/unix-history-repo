/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigprocmask.s	5.2 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

err:
	jmp	cerror

ENTRY(sigprocmask)
	tstl	8(ap)			# check new sigset pointer
	bneq	1f			# if not null, indirect
/*	movl	$0,8(ap)		# null mask pointer: block empty set */
	movl	$1,4(ap)		# SIG_BLOCK
	jbr	2f
1:	movl	*8(ap),8(ap)		# indirect to new mask arg
2:	chmk	$SYS_sigprocmask
	jcs	err
	tstl	12(ap)			# test if old mask requested
	beql	out
	movl	r0,*12(ap)		# store old mask
out:
	clrl	r0
	ret
