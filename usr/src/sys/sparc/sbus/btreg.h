/*
 * Copyright (c) 1993
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
 *	@(#)btreg.h	8.2 (Berkeley) %G%
 *
 * from: $Header: btreg.h,v 1.1 93/10/12 15:28:52 torek Exp $
 */

/*
 * Several Sun color frame buffers use some kind of Brooktree video
 * DAC (e.g., the Bt458, -- in any case, Brooktree make the only
 * decent color frame buffer chips).
 *
 * Color map control on these is a bit funky in a SPARCstation.
 * To update the color map one would normally do byte writes, but
 * the hardware takes longword writes.  Since there are three
 * registers for each color map entry (R, then G, then B), we have
 * to set color 1 with a write to address 0 (setting 0's R/G/B and
 * color 1's R) followed by a second write to address 1 (setting
 * color 1's G/B and color 2's R/G).  Software must therefore keep
 * a copy of the current map.
 *
 * The colormap address register increments automatically, so the
 * above write is done as:
 *
 *	bt->bt_addr = 0;
 *	bt->bt_cmap = R0G0B0R1;
 *	bt->bt_cmap = G1B1R2G2;
 *	...
 *
 * Yow!
 *
 * Bonus complication: on the cg6, only the top 8 bits of each 32 bit
 * register matter, even though the cg3 takes all the bits from all
 * bytes written to it.
 */
struct bt_regs {
	u_int	bt_addr;		/* map address register */
	u_int	bt_cmap;		/* colormap data register */
	u_int	bt_ctrl;		/* control register */
	u_int	bt_omap;		/* overlay (cursor) map register */
};
