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
 *	@(#)kn01.h	7.1 (Berkeley) %G%
 */

/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and
 * its documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS" 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND 
 * FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 *
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 *
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */
/*
 * HISTORY
 * $Log:	kn01.h,v $
 * Revision 2.5  91/05/14  17:23:14  mrt
 * 	Correcting copyright
 * 
 * Revision 2.4  91/02/05  17:41:54  mrt
 * 	Added author notices
 * 	[91/02/04  11:14:12  mrt]
 * 
 * 	Changed to use new Mach copyright
 * 	[91/02/02  12:12:45  mrt]
 * 
 * Revision 2.3  90/12/05  23:31:50  af
 * 	Cleanups.
 * 	[90/12/04  16:38:21  af]
 * 
 * 	Created, from the DEC specs:
 * 	"DECstation 3100 Desktop Workstation Functional Specification"
 * 	Workstation Systems Engineering, Palo Alto, CA. Aug 28, 1990.
 * 	[90/09/03            af]
 */
/*
 *	File: kn01.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	9/90
 *
 *	Definitions specific to the KN01 processor (pmax)
 */

#ifndef	MIPS_KN01_H
#define	MIPS_KN01_H	1

/*
 * KN01's Physical address space
 */

#define KN01_PHYS_MIN		0x00000000	/* 512 Meg */
#define KN01_PHYS_MAX		0x1fffffff

/*
 * Memory map
 */

#define KN01_PHYS_MEMORY_START	0x00000000
#define KN01_PHYS_MEMORY_END	0x01800000	/* 24 Meg in 8 slots */

#define	KN01_PHYS_FBUF_START	0x0fc00000	/* frame buffer memory */
#define	KN01_PHYS_FBUF_M_END	0x0fc20000	/* mono */
#define	KN01_PHYS_FBUF_C_END	0x0fd00000	/* color */

#define	KN01_PHYS_COLMASK_START	0x10000000	/* Color Plane mask */
#define	KN01_PHYS_COLMASK_END	0x11000000	/* Color Plane mask */


/*
 * I/O map
 */

#define KN01_SYS_PCC		0x11000000	/* Progr. Cursor Chip */

#define KN01_SYS_VDAC		0x12000000	/* Color map */

#define KN01_SYS_ERRADR		0x17000000	/* Write error address */

#define KN01_SYS_LANCE		0x18000000	/* Lance chip */

#define KN01_SYS_LANCE_B_START	0x19000000	/* 64 Kb Lance Buffer */
#define KN01_SYS_LANCE_B_END	0x19010000

#define KN01_SYS_SII		0x1a000000	/* scsi SII chip */

#define KN01_SYS_SII_B_START	0x1b000000	/* 128 Kb SCSI buffer */
#define KN01_SYS_SII_B_END	0x1b020000

#define	KN01_SYS_DZ		0x1c000000	/* Serial lines (DZ) */

#define	KN01_SYS_CLOCK		0x1d000000	/* rtc chip */

#define	KN01_SYS_CSR		0x1e000000	/* System control register */

#define	KN01_SYS_ROM_START	0x1f000000	/* System ROM */
#define	KN01_SYS_ROM_END	0x1f07ffff


/*
 * Interrupts
 */

#define KN01_INT_FPA		IP_LEV7		/* Floating Point coproc */
#define KN01_INT_MEM		IP_LEV6		/* memory controller */
#define KN01_INT_CLOCK		IP_LEV5		/* rtc chip */
#define KN01_INT_DZ		IP_LEV4		/* serial line chip */
#define KN01_INT_LANCE		IP_LEV3		/* ether */
#define KN01_INT_SII		IP_LEV2		/* SCSI interface */

/*
 * System board registers
 */

/* system Status and Control register */

#define KN01_CSR_LEDS_MASK	0x00ff		/* wo */
						/* Diagnostic leds mask */
#define KN01_CSR_VRGTRB		0x0001		/* ro */
						/* Red VoltageLev > Blue */
#define KN01_CSR_VRGTRG		0x0002		/* ro */
						/* Red VoltageLev > Green */
#define KN01_CSR_VBGTRG		0x0004		/* ro */
						/* Blue VoltageLev > Green */
#define KN01_CSR_TXDIS		0x0100		/* rw */
						/* Disable DZ xmit */
#define KN01_CSR_VINT		0x0200		/* rc */
						/* Vertical retrace intr. */
#define KN01_CSR_MERR		0x0400		/* rc */
						/* Memory write error intr */
#define KN01_CSR_MONO		0x0800		/* ro */
						/* Mono Framebuf (or none) */
#define KN01_CSR_CRSRTST	0x1000		/* ro */
						/* Cursor test output */
#define KN01_CSR_PARDIS		0x2000		/* rw */
						/* Disable mem parity chks */ 
#define KN01_CSR_SELFTEST	0x4000		/* rw */
						/* Self-test ok pinout */
#define KN01_CSR_MNFMOD		0x8000		/* ro */
						/* Manifacturer MOD jumper */
#define	KN01_CSR_MBZ		0x9800

/* Error address status register */

#define KN01_ERR_ADDRESS	0x07ffffff	/* phys address mask ? */


#endif	MIPS_KN01_H
