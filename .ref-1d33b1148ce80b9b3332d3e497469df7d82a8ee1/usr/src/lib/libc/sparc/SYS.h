/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)SYS.h	8.1 (Berkeley) %G%
 *
 * from: $Header: SYS.h,v 1.2 92/07/03 18:57:00 torek Exp $
 */

#include <sys/syscall.h>
#include <machine/trap.h>

#ifdef PROF
#define	ENTRY(x) \
	.align 4; .globl _##x; .proc 1; _##x:; .data; .align 4; 1: .long 0; \
	.text; save %sp,-96,%sp; sethi %hi(1b),%o0; call mcount; \
	or %o0,%lo(1b),%o0; restore
#else
#define	ENTRY(x) \
	.align 4; .globl _##x; .proc 1; _##x:
#endif

/*
 * ERROR branches to cerror.  This is done with a macro so that I can
 * change it to be position independent later, if need be.
 */
#define	ERROR() \
	sethi %hi(cerror),%g1; or %lo(cerror),%g1,%g1; jmp %g1; nop

/*
 * SYSCALL is used when further action must be taken before returning.
 * Note that it adds a `nop' over what we could do, if we only knew what
 * came at label 1....
 */
#define	SYSCALL(x) \
	ENTRY(x); mov SYS_##x,%g1; t ST_SYSCALL; bcc 1f; nop; ERROR(); 1:

/*
 * RSYSCALL is used when the system call should just return.  Here
 * we use the SYSCALL_G2RFLAG to put the `success' return address in %g2
 * and avoid a branch.
 */
#define	RSYSCALL(x) \
	ENTRY(x); mov (SYS_##x)|SYSCALL_G2RFLAG,%g1; add %o7,8,%g2; \
	t ST_SYSCALL; ERROR()

/*
 * PSEUDO(x,y) is like RSYSCALL(y) except that the name is x.
 */
#define	PSEUDO(x,y) \
	ENTRY(x); mov (SYS_##y)|SYSCALL_G2RFLAG,%g1; add %o7,8,%g2; \
	t ST_SYSCALL; ERROR()

#define	ASMSTR		.asciz

	.globl	cerror
