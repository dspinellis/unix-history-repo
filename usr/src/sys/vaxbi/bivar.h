/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)bivar.h	7.3 (Berkeley) 6/28/90
 */

/*
 * Software status per BI node.
 */

struct	binode_status {
/*	int	bh_type;		/* type of adapter */
	struct	biiregs *bh_bi;		/* virt addr of registers */
	struct	biiregs *bh_physbi;	/* phys addr of registers */
	int	bh_errcnt;		/* number of errors */
	int	bh_ivec;		/* interrupt vector */
	int	bh_arb;			/* arbitration */
};

/*
 * ... per BI
 */
struct	bi_hd {
	short	bh_nodes;		/* which nodes are present */
	struct	binode_status bh_nodes[16];	/* info about those nodes */
};
