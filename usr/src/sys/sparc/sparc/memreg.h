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
 *	@(#)memreg.h	7.3 (Berkeley) %G%
 *
 * from: $Header: memreg.h,v 1.6 92/11/26 03:05:05 torek Exp $ (LBL)
 */

/*
 * Sun-4c memory error register.
 * The register is a single word.
 */
volatile int	*par_err_reg;	/* virtual address; NULL if not yet mapped */

/*
 * Bits in parity error register.
 * The register is cleared when read, except for the test and enable bits.
 */
#define	PER_ERR		0x80	/* a parity error occurred */
#define	PER_MULTI	0x40	/* more than one occurred */
#define	PER_TEST	0x20	/* test (invert parity) */
#define	PER_ENABLE	0x10	/* enable parity error reports */
#define	PER_BYTE0	0x08	/* error occurred in byte 0 (bits 31..24) */
#define	PER_BYTE1	0x04	/* error occurred in byte 1 (bits 23..16) */
#define	PER_BYTE2	0x02	/* error occurred in byte 2 (bits 15..8) */
#define	PER_BYTE3	0x01	/* error occurred in byte 3 (bits 7..0) */

#define	PER_BITS "\20\10ERR\7MULTI\6TEST\5ENABLE\4BYTE0\3BYTE1\2BYTE2\1BYTE3"
