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
 *	@(#)DEFS.h	8.1 (Berkeley) %G%
 */

#ifdef PROF
#define	FUNC(x) \
	.align 4; .globl x; .proc 1; x:; .data; .align 4; 1: .long 0; \
	.text; save %sp,-96,%sp; sethi %hi(1b),%o0; call mcount; \
	or %lo(1b),%o0,%o0; restore
#else
#define	FUNC(x) \
	.align 4; .globl x; .proc 1; x:
#endif

#define	ENTRY(x) FUNC(_##x)
