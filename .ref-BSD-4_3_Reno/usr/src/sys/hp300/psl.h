/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)psl.h	7.1 (Berkeley) 5/8/90
 */

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
#define	PSL_S		0x2000		/* supervisor enable bit */
#define	PSL_T		0x8000		/* trace enable bit */

#define	PSL_LOWIPL	(PSL_S)
#define	PSL_HIGHIPL	(PSL_S | PSL_IPL7)
#define PSL_IPL		(PSL_IPL7)
#define	PSL_USER	(0)

#define	PSL_MBZ		0x58E0		/* must be zero bits */

#define	PSL_USERSET	(0)
#define	PSL_USERCLR	(PSL_S | PSL_IPL7 | PSL_MBZ)
