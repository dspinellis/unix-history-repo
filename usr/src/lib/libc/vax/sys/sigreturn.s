/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)sigreturn.s	5.1 (Berkeley) %G%";
#endif not lint

#include "SYS.h"

/*
 * We must preserve the state of the registers as the user has set them up.
 */
#ifdef PROF
#undef ENTRY
#define	ENTRY(x) \
	.globl _/**/x; .align 2; _/**/x: .word 0; pushr $0x3f; \
	.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount; popr $0x3f
#endif PROF

SYSCALL(sigreturn)
	ret
