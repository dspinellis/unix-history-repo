/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)psl.h	5.2 (Berkeley) %G%
 */

/*
 * 386 processor status longword.
 */
#define	PSL_C		0x00000001	/* carry bit */
#define	PSL_PF		0x00000004	/* parity bit */
#define	PSL_AF		0x00000010	/* bcd carry bit */
#define	PSL_Z		0x00000040	/* zero bit */
#define	PSL_N		0x00000080	/* negative bit */
#define	PSL_ALLCC	0x000000d5	/* all cc bits - unlikely */
#define	PSL_T		0x00000100	/* trace enable bit */
#define	PSL_I		0x00000200	/* interrupt enable bit */
#define	PSL_D		0x00000400	/* string instruction direction bit */
#define	PSL_V		0x00000800	/* overflow bit */
#define	PSL_IOPL	0x00003000	/* i/o priviledge level enable */
#define	PSL_NT		0x00004000	/* nested task bit */
#define	PSL_RF		0x00010000	/* restart flag bit */
#define	PSL_VM		0x00020000	/* virtual 8086 mode bit */

#define	PSL_MBZ		0xfffc7fb7	/* must be zero bits */
#define	PSL_MBO		0x00000002	/* must be one bits */

#define	PSL_USERSET	(PSL_IOPL)
#define	PSL_USERCLR	(PSL_I|PSL_NT)
