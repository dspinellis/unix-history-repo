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
 *	@(#)kn02.h	7.1 (Berkeley) %G%
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
 * $Log:	kn02.h,v $
 * Revision 2.5  91/05/14  17:23:30  mrt
 * 	Correcting copyright
 * 
 * Revision 2.4  91/02/05  17:42:03  mrt
 * 	Added author notices
 * 	[91/02/04  11:14:23  mrt]
 * 
 * 	Changed to use new Mach copyright
 * 	[91/02/02  12:12:58  mrt]
 * 
 * Revision 2.3  90/12/05  23:32:04  af
 * 
 * 
 * Revision 2.1.1.2  90/11/01  02:48:10  af
 * 	Reworked a bit, made reentrant.
 * 
 * Revision 2.1.1.1  90/10/03  11:48:22  af
 * 	Created, from the DEC specs:
 * 	"DECstation 5000/200 KN02 System Module Functional Specification"
 * 	Workstation Systems Engineering, Palo Alto, CA. Aug 27, 1990.
 * 	[90/09/03            af]
 */
/*
 *	File: kn02.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	9/90
 *
 *	Definitions specific to the KN02 processor (3max)
 */

#ifndef	MIPS_KN02_H
#define	MIPS_KN02_H 1

/*
 * KN02's Physical address space
 */

#define KN02_PHYS_MIN		0x00000000	/* 512 Meg */
#define KN02_PHYS_MAX		0x1fffffff

/*
 * Memory map
 */

#define KN02_PHYS_MEMORY_START	0x00000000
#define KN02_PHYS_MEMORY_END	0x1dffffff	/* 480 Meg in 15 slots */

/*
 * I/O map
 */

#define KN02_PHYS_TC_0_START	0x1e000000	/* TURBOchannel, slot 0 */
#define KN02_PHYS_TC_0_END	0x1e3fffff	/*   4 Meg, option0 */

#define KN02_PHYS_TC_1_START	0x1e400000	/* TURBOchannel, slot 1 */
#define KN02_PHYS_TC_1_END	0x1e7fffff	/*   4 Meg, option1 */

#define KN02_PHYS_TC_2_START	0x1e800000	/* TURBOchannel, slot 2 */
#define KN02_PHYS_TC_2_END	0x1ebfffff	/*   4 Meg, option2 */

#define KN02_PHYS_TC_3_START	0x1ec00000	/* TURBOchannel, slot 3 */
#define KN02_PHYS_TC_3_END	0x1effffff	/*   4 Meg, reserved*/

#define KN02_PHYS_TC_4_START	0x1f000000	/* TURBOchannel, slot 4 */
#define KN02_PHYS_TC_4_END	0x1f3fffff	/*   4 Meg, reserved*/

#define KN02_PHYS_TC_5_START	0x1f400000	/* TURBOchannel, slot 5 */
#define KN02_PHYS_TC_5_END	0x1f7fffff	/*   4 Meg, SCSI */

#define KN02_PHYS_TC_6_START	0x1f800000	/* TURBOchannel, slot 6 */
#define KN02_PHYS_TC_6_END	0x1fbfffff	/*   4 Meg, ether */

#define KN02_PHYS_TC_7_START	0x1fc00000	/* TURBOchannel, slot 7 */
#define KN02_PHYS_TC_7_END	0x1fffffff	/*   4 Meg, system devices */

#define	KN02_PHYS_TC_START	KN02_PHYS_TC_0_START
#define	KN02_PHYS_TC_END	KN02_PHYS_TC_7_END	/* 32 Meg */

#define KN02_TC_NSLOTS		8
#define	KN02_TC_MIN		0
#define KN02_TC_MAX		6		/* don't look at system slot */

/*
 * System devices
 */

#define	KN02_SYS_ROM_START	KN02_PHYS_TC_7_START+0x000000
#define	KN02_SYS_ROM_END	KN02_PHYS_TC_7_START+0x07ffff

#define KN02_SYS_RESERVED	KN02_PHYS_TC_7_START+0x080000

#define	KN02_SYS_CHKSYN		KN02_PHYS_TC_7_START+0x100000

#define	KN02_SYS_ERRADR		KN02_PHYS_TC_7_START+0x180000

#define	KN02_SYS_DZ		KN02_PHYS_TC_7_START+0x200000

#define	KN02_SYS_CLOCK		KN02_PHYS_TC_7_START+0x280000

#define	KN02_SYS_CSR		KN02_PHYS_TC_7_START+0x300000

#define	KN02_SYS_ROM1_START	KN02_PHYS_TC_7_START+0x380000
#define	KN02_SYS_ROM1_END	KN02_PHYS_TC_7_START+0x3fffff


/*
 * Interrupts
 */

#define KN02_INT_FPA		IP_LEV7		/* Floating Point coproc */
#define KN02_INT_RES1		IP_LEV6		/* reserved, unused */
#define KN02_INT_MEM		IP_LEV5		/* memory controller */
#define KN02_INT_RES2		IP_LEV4		/* reserved, unused */
#define KN02_INT_CLOCK		IP_LEV3		/* rtc chip */
#define KN02_INT_IO		IP_LEV2		/* I/O slots */

/*
 * System board registers
 */

/* system Status and Control register */

#define KN02_CSR_IOINT		0x000000ff	/* ro */
						/* Interrupt pending */
#	define KN02_IP_DZ	0x00000080	/* serial lines */
#	define KN02_IP_LANCE	0x00000040	/* thin ethernet */
#	define KN02_IP_SCSI	0x00000020	/* ASC scsi controller */
#	define KN02_IP_XXXX	0x00000018	/* unused */
#	define KN02_IP_SLOT2	0x00000004	/* option slot 2 */
#	define KN02_IP_SLOT1	0x00000002	/* option slot 1 */
#	define KN02_IP_SLOT0	0x00000001	/* option slot 0 */

#define KN02_CSR_BAUD38		0x00000100	/* rw */
						/* Max DZ baud rate */
#define KN02_CSR_DIAGDN		0x00000200	/* rw */
						/* Diag jumper */
#define KN02_CSR_BNK32M		0x00000400	/* rw */
						/* Memory bank stride */
#define KN02_CSR_TXDIS		0x00000800	/* rw */
						/* Disable DZ xmit */
#define KN02_CSR_LEDIAG		0x00001000	/* rw */
						/* Latch ECC */
#define KN02_CSR_CORRECT	0x00002000	/* rw */
						/* ECC corrects single bit */
#define KN02_CSR_ECCMD		0x0000c000	/* rw */
						/* ECC logic mode */
#define KN02_CSR_IOINTEN	0x00ff0000	/* rw */
#define	KN02_CSR_IOINTEN_SHIFT	16	/* Interrupt enable */

#define KN02_CSR_NRMMOD		0x01000000	/* ro */
						/* Diag jumper state */
#define KN02_CSR_REFEVEN	0x02000000	/* ro */
						/* Refreshing even mem bank */
#define KN02_CSR_PRSVNVR	0x04000000	/* ro */
						/* Preserve NVR jumper */
#define KN02_CSR_PSWARN		0x08000000	/* ro */
						/* PS overheating */
#define KN02_CSR_RRESERVED	0xf0000000	/* rz */

#define KN02_CSR_LEDS		0x000000ff	/* wo */
						/* Diag LEDs */
#define KN02_CSR_WRESERVED	0xff000000	/* wz */


/* Error address status register */

#define KN02_ERR_ADDRESS	0x07ffffff	/* phys address */
#define KN02_ERR_RESERVED	0x08000000	/* unused */
#define KN02_ERR_ECCERR		0x10000000	/* ECC error */
#define KN02_ERR_WRITE		0x20000000	/* read/write transaction */
#define KN02_ERR_CPU		0x40000000	/* CPU or device initiator */
#define KN02_ERR_VALID		0x80000000	/* Info is valid */

/* ECC check/syndrome status register */

#define KN02_ECC_SYNLO		0x0000007f	/* syndrome, even bank	*/
#define KN02_ECC_SNGLO		0x00000080	/* single bit err, " 	*/
#define KN02_ECC_CHKLO		0x00007f00	/* check bits,	"  "	*/
#define KN02_ECC_VLDLO		0x00008000	/* info valid for  "	*/
#define KN02_ECC_SYNHI		0x007f0000	/* syndrome, odd bank	*/
#define KN02_ECC_SNGHI		0x00800000	/* single bit err, "	*/
#define KN02_ECC_CHKHI		0x7f000000	/* check bits,  "  "	*/
#define KN02_ECC_VLDHI		0x80000000	/* info valid for  "	*/


#endif	MIPS_KN02_H
