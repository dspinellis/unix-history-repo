/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)proc.h	7.4 (Berkeley) %G%
 */

/*
 * Machine-dependent part of the proc structure for hp300.
 */
struct mdproc {
	int	*md_regs;		/* registers on current frame */
	int	md_flags;		/* machine-dependent flags */
};

/* md_flags */
#define	MDP_AST		0x0001	/* async trap pending */
#define	MDP_HPUX	0x0002	/* HP-UX process */
#define	MDP_HPUXTRACE	0x0004	/* being traced by HP-UX process */
#define	MDP_HPUXMMAP	0x0008	/* VA space is multiply mapped */
#define MDP_CCBDATA	0x0010	/* copyback caching of data (68040) */
#define MDP_CCBSTACK	0x0020	/* copyback caching of stack (68040) */
