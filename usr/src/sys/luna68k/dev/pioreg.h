/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pioreg.h	7.1 (Berkeley) %G%
 */

/*
 * PIO definitions
 * OMRON: $Id: pioreg.h,v 1.1 92/05/27 14:33:18 moti Exp $
 * by Shigeto Mochida 
 */

#define	PIO0_ADDR      	0x49000000	/* pio0 address */
#define	PIO1_ADDR	0x4D000000	/* pio1 address */

#define	PIO_MODED	0xB6		/* pio mode set */

struct pio {
    unsigned char	a_port;
    unsigned char	b_port;
    unsigned char	c_port;
    unsigned char	control_port;
};
