/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)cgthreereg.h	8.2 (Berkeley) %G%
 *
 * from: $Header: cgthreereg.h,v 1.7 93/10/31 05:09:28 torek Exp $
 */

/*
 * cgthree display registers.  Much like bwtwo registers, except that
 * there is a Brooktree Video DAC in there (so we also use btreg.h).
 */

/* offsets */
#define	CG3REG_ID	0
#define	CG3REG_REG	0x400000
#define	CG3REG_MEM	0x800000

/* same, but for gdb */
struct cgthree_all {
	long	ba_id;			/* ID = 0xfe010104 on my IPC */
	char	ba_xxx0[0x400000-4];
	struct	bt_regs ba_btreg;	/* Brooktree registers */
	char	ba_xxx1[0x400000-sizeof(struct bt_regs)];
	char	ba_ram[4096];		/* actually larger */
};
