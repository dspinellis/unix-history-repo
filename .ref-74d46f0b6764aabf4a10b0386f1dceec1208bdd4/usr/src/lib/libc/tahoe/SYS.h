/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)SYS.h	5.6 (Berkeley) %G%
 */

#include <sys/syscall.h>

#ifdef PROF
#define	ENTRY(x)	.globl _/**/x; .align 2; _/**/x: .word 0; \
			.data; 1:; .long 0; .text; pushl $1b; callf $8,mcount
#else
#define	ENTRY(x)	.globl _/**/x; .align 2; _/**/x: .word 0
#endif PROF
#define	SYSCALL(x)	err: jmp cerror; ENTRY(x); kcall $SYS_/**/x; jcs err
#define	PSEUDO(x,y)	ENTRY(x); kcall $SYS_/**/y
#define	CALL(x,y)	calls $x, _/**/y

#define	ASMSTR		.asciz

	.globl	cerror
