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
 *	@(#)ptrace.h	7.3 (Berkeley) %G%
 *
 * from: $Header: ptrace.h,v 1.6 92/11/26 02:04:43 torek Exp $ (LBL)
 */

/*
 * SPARC-dependent ptrace definitions.
 */
#define	PT_GETREGS	(PT_FIRSTMACH + 0)
#define	PT_SETREGS	(PT_FIRSTMACH + 1)
#define	PT_GETFPREGS	(PT_FIRSTMACH + 2)
#define	PT_SETFPREGS	(PT_FIRSTMACH + 3)

