/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)profile.h	7.3 (Berkeley) %G%
 *
 * from: $Header: profile.h,v 1.8 92/11/26 02:04:41 torek Exp $
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
