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
 *	@(#)bwtworeg.h	7.3 (Berkeley) %G%
 *
 * from: $Header: bwtworeg.h,v 1.4 92/11/26 02:28:05 torek Exp $
 */

/*
 * bwtwo display registers.
 *
 * The registers start at offset 0x400000 and repeat every 32 bytes
 * (presumably only the low order address lines are decoded).  Video RAM
 * starts at offset 0x800000.  We use separate pointers to each so that
 * the sparc addressing modes work well.
 */
struct bwtworeg {
	/*
	 * The xxx0 range is all 0xff on my IPC but causes a screen glitch
	 * on my SS1+, so it must do *some*thing... the xxx1 range is full
	 * of values but I do not know what they are.  bw_ctl changes for
	 * a blanked screen.
	 */
	char	bw_xxx0[16];
	u_char	bw_ctl;			/* contains video enable */
	char	bw_xxx1[15];
};

/* bits in bw_ctl */
#define	CTL_VE	0x40			/* video enable */

/* offsets */
#define	BWREG_ID	0
#define	BWREG_REG	0x400000
#define	BWREG_MEM	0x800000

/* same, but for gdb */
struct bwtwo_all {
	long	ba_id;			/* ID = 0xfe010104 on my IPC */
	char	ba_xxx0[0x400000-4];
	struct	bwtworeg ba_reg;	/* control registers */
	char	ba_xxx1[0x400000-32];
	char	ba_ram[4096];		/* actually larger */
};
