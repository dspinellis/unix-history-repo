/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)SYS.h	8.1 (Berkeley) %G%
 */

#include <sys/syscall.h>

#ifdef __STDC__
#ifdef PROF
#if __GNUC__ >= 2
#define	ENTRY(x)	.globl _ ## x; .even; _ ## x:; .data; PROF ## x:; \
			.long 0; .text; link a6,\#0; lea PROF ## x,a0; \
			jbsr mcount; unlk a6
#else
#define	ENTRY(x)	.globl _ ## x; .even; _ ## x:; .data; PROF ## x:; \
			.long 0; .text; link a6,#0; lea PROF ## x,a0; \
			jbsr mcount; unlk a6
#endif
#else
#define	ENTRY(x)	.globl _ ## x; .even; _ ## x:
#endif
#if __GNUC__ >= 2
#define SYSTRAP(x)	movl \#SYS_ ## x,d0; trap \#0
#else
#define SYSTRAP(x)	movl #SYS_ ## x,d0; trap #0
#endif
#else
#ifdef PROF
#define	ENTRY(x)	.globl _/**/x; .even; _/**/x:; .data; PROF/**/x:; \
			.long 0; .text; link a6,#0; lea PROF/**/x,a0; \
			jbsr mcount; unlk a6
#else
#define	ENTRY(x)	.globl _/**/x; .even; _/**/x:
#endif
#define SYSTRAP(x)	movl #SYS_/**/x,d0; trap #0
#endif

#define	SYSCALL(x)	.even; err: jmp cerror; ENTRY(x); SYSTRAP(x); jcs err
#define	RSYSCALL(x)	SYSCALL(x); rts
#define	PSEUDO(x,y)	ENTRY(x); SYSTRAP(y); rts

#define	ASMSTR		.asciz

	.globl	cerror
