/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University,
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmaxtype.h	7.1 (Berkeley) %G%
 */

/*
 * Mother board type byte of "systype" environment variable.
 */
#define	DS_PMAX		0x1	/* DECstation 2100/3100 */
#define	DS_3MAX		0x2	/* DECstation 5000/200 */
#define	DS_3MIN		0x3	/* DECstation 5000/1xx */
#define	DS_LSIS		0x5	/* DECsystem 5800 */
#define	DS_MIPSFAIR	0x6	/* DECsystem 5400 */
#define	DS_MAXINE	0x7	/* Personal DECstation 5000/xx */
#define	DS_MIPSFAIR2	0xb	/* DECsystem 5500 */
#define	DS_MIPSMATE	0xc	/* DECsystem 5100 */
