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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ptrace.h	7.2 (Berkeley) %G%
 *
 * from: $Header: ptrace.h,v 1.5 92/06/17 06:10:26 torek Exp $ (LBL)
 */

/*
 * SPARC-dependent ptrace definitions.
 */
#define	PT_GETREGS	(PT_FIRSTMACH + 0)
#define	PT_SETREGS	(PT_FIRSTMACH + 1)
#define	PT_GETFPREGS	(PT_FIRSTMACH + 2)
#define	PT_SETFPREGS	(PT_FIRSTMACH + 3)

