/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)psl.h	7.4 (Berkeley) %G%
 */

#ifndef PSL_C
/*
 * MC68000 program status word
 */

#define	PSL_C		0x0001		/* carry bit */
#define	PSL_V		0x0002		/* overflow bit */
#define	PSL_Z		0x0004		/* zero bit */
#define	PSL_N		0x0008		/* negative bit */
#define	PSL_X		0x0010		/* extend bit */
#define	PSL_ALLCC	0x001F		/* all cc bits - unlikely */
#define	PSL_IPL0	0x0000		/* interrupt priority level 0 */
#define	PSL_IPL1	0x0100		/* interrupt priority level 1 */
#define	PSL_IPL2	0x0200		/* interrupt priority level 2 */
#define	PSL_IPL3	0x0300		/* interrupt priority level 3 */
#define	PSL_IPL4	0x0400		/* interrupt priority level 4 */
#define	PSL_IPL5	0x0500		/* interrupt priority level 5 */
#define	PSL_IPL6	0x0600		/* interrupt priority level 6 */
#define	PSL_IPL7	0x0700		/* interrupt priority level 7 */
#define	PSL_M		0x1000		/* master (kernel) sp vs intr sp */
#define	PSL_S		0x2000		/* supervisor enable bit */
/*	PSL_T0		0x4000		   ??? T0 on 68020, 8000 is T1 */
#define	PSL_T		0x8000		/* trace enable bit */

#define	PSL_LOWIPL	(PSL_S)
#define	PSL_HIGHIPL	(PSL_S | PSL_IPL7)
#define PSL_IPL		(PSL_IPL7)
#define	PSL_USER	(0)

#define	PSL_MBZ		0xFFFF58E0	/* must be zero bits */

#define	PSL_USERSET	(0)
#define	PSL_USERCLR	(PSL_S | PSL_IPL7 | PSL_MBZ)

#define	USERMODE(ps)	(((ps) & PSL_S) == 0)
#endif
