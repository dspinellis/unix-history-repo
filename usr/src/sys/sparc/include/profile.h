/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)profile.h	7.1 (Berkeley) %G%
 *
 * from: $Header: profile.h,v 1.7 92/07/09 01:34:21 mccanne Exp $
 */

#define MCOUNT \
        asm(".global mcount");\
        asm("mcount:");\
        asm("add %i7, 8, %o0");\
        asm("sethi %hi(__mcount), %o2");\
        asm("jmpl %o2 + %lo(__mcount), %g0");\
        asm("add %o7, 8, %o1");

#define	_MCOUNT_DECL	static void _mcount

#ifdef KERNEL
/*
 * Block interrupts during mcount so that those interrupts can also be
 * counted (as soon as we get done with the current counting).  On the
 * SPARC, we just splhigh/splx as those do not recursively invoke mcount.
 */
#define	MCOUNT_ENTER	s = splhigh()
#define	MCOUNT_EXIT	splx(s)
#endif /* KERNEL */
