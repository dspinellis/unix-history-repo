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
	.asciz "@(#)sigreturn.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

/*
 * We must preserve the state of the registers as the user has set them up.
 */
#ifdef PROF
#undef ENTRY
#define	ENTRY(x) \
	.globl _/**/x; .even; _/**/x:; moveml #0xC0C0,sp@-; .data; \
	PROF/**/x:; .long 0; .text; lea PROF/**/x,a0; jbsr mcount; \
	moveml sp@+,#0x0303
#endif PROF

ENTRY(sigreturn)
	trap	#1		/* signals sigreturn() */
	jmp	cerror
