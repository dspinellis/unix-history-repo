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
 *	@(#)fsr.h	7.3 (Berkeley) %G%
 *
 * from: $Header: fsr.h,v 1.6 92/11/26 02:04:36 torek Exp $
 */

#ifndef _MACHINE_FSR_H_
#define	_MACHINE_FSR_H_

/*
 * Bits in FSR.
 */
#define	FSR_RD		0xc0000000	/* rounding direction */
#define	  FSR_RD_RN	0		/* round to nearest */
#define	  FSR_RD_RZ	1		/* round towards 0 */
#define	  FSR_RD_RP	2		/* round towards +inf */
#define	  FSR_RD_RM	3		/* round towards -inf */
#define	FSR_RD_SHIFT	30
#define	FSR_RD_MASK	0x03

#define	FSR_RP		0x30000000	/* extended rounding precision */
#define	  FSR_RP_X	0		/* extended stays extended */
#define	  FSR_RP_S	1		/* extended => single */
#define	  FSR_RP_D	2		/* extended => double */
#define	  FSR_RP_80	3		/* extended => 80-bit */
#define	FSR_RP_SHIFT	28
#define	FSR_RP_MASK	0x03

#define	FSR_TEM		0x0f800000	/* trap enable mask */
#define	FSR_TEM_SHIFT	23
#define	FSR_TEM_MASK	0x1f

#define	FSR_NS		0x00400000	/* ``nonstandard mode'' */
#define	FSR_AU		0x00400000	/* aka abrupt underflow mode */
#define	FSR_MBZ		0x00300000	/* reserved; must be zero */

#define	FSR_VER		0x000e0000	/* version bits */
#define	FSR_VER_SHIFT	17
#define	FSR_VER_MASK	0x07

#define	FSR_FTT		0x0001c000	/* FP trap type */
#define	  FSR_TT_NONE	0		/* no trap */
#define	  FSR_TT_IEEE	1		/* IEEE exception */
#define	  FSR_TT_UNFIN	2		/* unfinished operation */
#define	  FSR_TT_UNIMP	3		/* unimplemented operation */
#define	  FSR_TT_SEQ	4		/* sequence error */
#define	  FSR_TT_HWERR	5		/* hardware error (unrecoverable) */
#define	FSR_FTT_SHIFT	14
#define	FSR_FTT_MASK	0x03

#define	FSR_QNE		0x00002000	/* queue not empty */
#define	FSR_PR		0x00001000	/* partial result */

#define	FSR_FCC		0x00000c00	/* FP condition codes */
#define	  FSR_CC_EQ	0		/* f1 = f2 */
#define	  FSR_CC_LT	1		/* f1 < f2 */
#define	  FSR_CC_GT	2		/* f1 > f2 */
#define	  FSR_CC_UO	3		/* (f1,f2) unordered */
#define	FSR_FCC_SHIFT	10
#define	FSR_FCC_MASK	0x03

#define	FSR_AX	0x000003e0		/* accrued exceptions */
#define	  FSR_AX_SHIFT	5
#define	  FSR_AX_MASK	0x1f
#define	FSR_CX	0x0000001f		/* current exceptions */
#define	  FSR_CX_SHIFT	0
#define	  FSR_CX_MASK	0x1f

/* The following exceptions apply to TEM, AX, and CX. */
#define	FSR_NV	0x10			/* invalid operand */
#define	FSR_OF	0x08			/* overflow */
#define	FSR_UF	0x04			/* underflow */
#define	FSR_DZ	0x02			/* division by zero */
#define	FSR_NX	0x01			/* inexact result */

#endif /* _MACHINE_FSR_H_ */
