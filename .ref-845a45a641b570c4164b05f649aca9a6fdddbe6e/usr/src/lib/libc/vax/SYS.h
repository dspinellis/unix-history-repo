/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)SYS.h	8.1 (Berkeley) %G%
 */

#include <sys/syscall.h>

#ifdef PROF
#define	ENTRY(x)	.globl _/**/x; .align 2; _/**/x: .word 0; \
			.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount
#else
#define	ENTRY(x)	.globl _/**/x; .align 2; _/**/x: .word 0
#endif PROF
#define	SYSCALL(x)	err: jmp cerror; ENTRY(x); chmk $SYS_/**/x; jcs err
#define	RSYSCALL(x)	SYSCALL(x); ret
#define	PSEUDO(x,y)	ENTRY(x); chmk $SYS_/**/y; ret
#define	CALL(x,y)	calls $x, _/**/y

#define	ASMSTR		.asciz

	.globl	cerror
