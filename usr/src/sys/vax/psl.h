/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)psl.h	7.2 (Berkeley) 12/10/87
 */

/*
 * VAX program status longword
 */

#define	PSL_C		0x00000001	/* carry bit */
#define	PSL_V		0x00000002	/* overflow bit */
#define	PSL_Z		0x00000004	/* zero bit */
#define	PSL_N		0x00000008	/* negative bit */
#define	PSL_ALLCC	0x0000000f	/* all cc bits - unlikely */
#define	PSL_T		0x00000010	/* trace enable bit */
#define	PSL_IV		0x00000020	/* integer overflow enable bit */
#define	PSL_FU		0x00000040	/* floating point underflow enable */
#define	PSL_DV		0x00000080	/* decimal overflow enable bit */
#define	PSL_IPL		0x001f0000	/* interrupt priority level */
#define	PSL_PRVMOD	0x00c00000	/* previous mode (all on is user) */
#define	PSL_CURMOD	0x03000000	/* current mode (all on is user) */
#define	PSL_IS		0x04000000	/* interrupt stack */
#define	PSL_FPD		0x08000000	/* first part done */
#define	PSL_TP		0x40000000	/* trace pending */
#define	PSL_CM		0x80000000	/* compatibility mode */

#define	PSL_MBZ		0x3020ff00	/* must be zero bits */

#define	PSL_USERSET	(PSL_PRVMOD|PSL_CURMOD)
#define	PSL_USERCLR	(PSL_IS|PSL_IPL|PSL_MBZ)
#define	PSL_CM_CLR	(PSL_FPD|PSL_DV|PSL_FU|PSL_IV)
