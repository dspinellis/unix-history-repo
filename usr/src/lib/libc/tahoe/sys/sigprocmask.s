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
	.asciz "@(#)sigprocmask.s	5.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

err:
	jmp	cerror

ENTRY(sigprocmask)
	tstl	8(fp)			# check new sigset pointer
	bneq	1f			# if not null, indirect
	movl	$0,8(fp)		# null mask pointer: block empty set
	movl	$1,4(fp)		# SIG_BLOCK
	jbr	2f
1:	movl	*8(fp),8(fp)		# indirect to new mask arg
2:	kcall	$SYS_sigprocmask
	jcs	err
	tstl	12(fp)			# test if old mask requested
	beql	out
	movl	r0,*12(fp)		# store old mask
out:
	clrl	r0
	ret
