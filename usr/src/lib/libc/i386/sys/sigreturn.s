/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigreturn.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

/*
 * We must preserve the state of the registers as the user has set them up.
 */
#ifdef PROF
#undef ENTRY
#define	ENTRY(x) \
	.globl _/**/x; .align 2; _/**/x:  pusha ; \
	.data; 1:; .long 0; .text; movl $1b,%eax; call mcount; popa ; nop
#endif /* PROF */

SYSCALL(sigreturn)
	ret
