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
 *	@(#)maxine.h	7.1 (Berkeley) %G%
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
 * $Log:	maxine.h,v $
 * Revision 2.3  92/04/01  15:14:52  rpd
 * 	Defined pseudo slot for mappable timer.
 * 	[92/03/11  02:37:41  af]
 * 
 * Revision 2.2  92/03/02  18:34:28  rpd
 * 	Created, from the DEC specs:
 * 	"MAXine System Module Functional Specification"  Revision 1.2
 * 	Workstation Systems Engineering, Palo Alto, CA. July 15, 1991.
 * 	[92/01/17            af]
 * 
 */
/*
 *	File: maxine.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	1/92
 *
 *	Definitions specific to the MAXine system module (54-21325-01)
 *	and compatible processors (KN02BA).
 */

#ifndef	MIPS_XINE_H
#define	MIPS_XINE_H 1

/*
 * MAXine's Physical address space
 */

#define XINE_PHYS_MIN		0x00000000	/* 512 Meg */
#define XINE_PHYS_MAX		0x1fffffff

/*
 * Memory map
 */

#define XINE_PHYS_MEMORY_START	0x00000000
#define XINE_PHYS_MEMORY_END	0x027fffff	/* 40 Meg in 2 slots
						   and baseboard */

/*
 * I/O map
 */

#define	XINE_PHYS_CFB_START	0x08000000	/* Color Frame Buffer */
#define	XINE_PHYS_CFB_END	0x0bffffff	/*  64 Meg */

#define	XINE_PHYS_MREGS_START	0x0c000000	/* Memory control registers */
#define	XINE_PHYS_MREGS_END	0x0dffffff	/*  32 Meg */
#define	XINE_PHYS_CREGS_START	0x0e000000	/* CPU ASIC control regs */
#define	XINE_PHYS_CREGS_END	0x0fffffff	/*  32 Meg */

#define XINE_PHYS_TC_0_START	0x10000000	/* TURBOchannel, slot 0 */
#define XINE_PHYS_TC_0_END	0x13ffffff	/*  64 Meg, option0 */

#define XINE_PHYS_TC_1_START	0x14000000	/* TURBOchannel, slot 1 */
#define XINE_PHYS_TC_1_END	0x17ffffff	/*  64 Meg, option1 */

#define XINE_PHYS_TC_RESERVED	0x18000000	/* Unused slot 2 */
						/*  64 Meg */

#define XINE_PHYS_TC_3_START	0x1c000000	/* TURBOchannel, slot 3 */
#define XINE_PHYS_TC_3_END	0x1fffffff	/*  64 Meg, system devices */

#define	XINE_PHYS_TC_START	XINE_PHYS_TC_0_START
#define	XINE_PHYS_TC_END	XINE_PHYS_TC_3_END	/* 256 Meg */

#define XINE_TC_NSLOTS		4
#define	XINE_TC_MIN		0
#define XINE_TC_MAX		1		/* only option slots */

/* Pseudo-TCslots */
#define	XINE_FLOPPY_SLOT	2
#define	XINE_SCSI_SLOT		3
#define	XINE_LANCE_SLOT		4
#define	XINE_SCC0_SLOT		5
#define	XINE_DTOP_SLOT		6
#define	XINE_ISDN_SLOT		7
#define	XINE_CFB_SLOT		8
#define	XINE_ASIC_SLOT		9
#define	XINE_FRC_SLOT		10

/*
 * System module space
 */

#define	XINE_SYS_ASIC		(XINE_PHYS_TC_3_START + 0x0000000)

#define	XINE_SYS_ROM_START	(XINE_SYS_ASIC + ASIC_SLOT_0_START)

#define XINE_SYS_ASIC_REGS	(XINE_SYS_ASIC + ASIC_SLOT_1_START)

#define	XINE_SYS_ETHER_ADDRESS	(XINE_SYS_ASIC + ASIC_SLOT_2_START)

#define	XINE_SYS_LANCE		(XINE_SYS_ASIC + ASIC_SLOT_3_START)

#define	XINE_SYS_SCC_0		(XINE_SYS_ASIC + ASIC_SLOT_4_START)

#define	XINE_SYS_VDAC_HI	(XINE_SYS_ASIC + ASIC_SLOT_5_START)

#define	XINE_SYS_VDAC_LO	(XINE_SYS_ASIC + ASIC_SLOT_7_START)

#define	XINE_SYS_CLOCK		(XINE_SYS_ASIC + ASIC_SLOT_8_START)

#define	XINE_SYS_ISDN		(XINE_SYS_ASIC + ASIC_SLOT_9_START)

#define	XINE_SYS_DTOP		(XINE_SYS_ASIC + ASIC_SLOT_10_START)

#define	XINE_SYS_FLOPPY		(XINE_SYS_ASIC + ASIC_SLOT_11_START)

#define	XINE_SYS_SCSI		(XINE_SYS_ASIC + ASIC_SLOT_12_START)

#define	XINE_SYS_FLOPPY_DMA	(XINE_SYS_ASIC + ASIC_SLOT_13_START)

#define	XINE_SYS_SCSI_DMA	(XINE_SYS_ASIC + ASIC_SLOT_14_START)

#define	XINE_SYS_BOOT_ROM_START	(XINE_PHYS_TC_3_START + 0x3c00000)
#define	XINE_SYS_BOOT_ROM_END	(XINE_PHYS_TC_3_START + 0x3c40000)

/*
 * Interrupts
 */

#define XINE_INT_FPA		IP_LEV7		/* Floating Point coproc */
#define XINE_INT_HALTB		IP_LEV6		/* Halt keycode (DTOP) */
#define XINE_INT_TC3		IP_LEV5		/* TC slot 3, system */
#define XINE_INT_TIMEOUT	IP_LEV4		/* Timeout on I/O write */
#define XINE_INT_TOY		IP_LEV3		/* Clock chip */
#define XINE_INT_1_10_MS	IP_LEV2		/* Periodic interrupt */

/*
 *  System registers addresses (MREG and CREG space, and IO Control ASIC)
 */

#define	XINE_REG_CMR		0x0c000000	/* Color mask register */
#define	XINE_REG_MER		0x0c400000	/* Memory error register */
#define	XINE_REG_MSR		0x0c800000	/* Memory size register */
#define	XINE_REG_FCTR		0x0ca00000	/* 1us free running counter */
#define	XINE_REG_FI		0x0cc00000	/* FI signal polarity (1!) */

#define	XINE_REG_CNFG		0x0e000000	/* Config mem timeouts */
#define	XINE_REG_AER		0x0e000004	/* Address error register */
#define	XINE_REG_TIMEOUT	0x0e00000c	/* I/O write timeout reg */


#define	XINE_REG_SCSI_DMAPTR	( XINE_SYS_ASIC + ASIC_SCSI_DMAPTR )
#define	XINE_REG_SCSI_DMANPTR	( XINE_SYS_ASIC + ASIC_SCSI_NEXTPTR )
#define	XINE_REG_LANCE_DMAPTR	( XINE_SYS_ASIC + ASIC_LANCE_DMAPTR )
#define	XINE_REG_SCC_T1_DMAPTR	( XINE_SYS_ASIC + ASIC_SCC_T1_DMAPTR )
#define	XINE_REG_SCC_R1_DMAPTR	( XINE_SYS_ASIC + ASIC_SCC_R1_DMAPTR )
#define	XINE_REG_DTOP_T_DMAPTR	( XINE_SYS_ASIC + ASIC_SCC_T2_DMAPTR )
#define	XINE_REG_DTOP_R_DMAPTR	( XINE_SYS_ASIC + ASIC_SCC_R2_DMAPTR )
#define	XINE_FLOPPY_DMAPTR	( XINE_SYS_ASIC + ASIC_FLOPPY_DMAPTR )
#define	XINE_ISDN_X_DMAPTR	( XINE_SYS_ASIC + ASIC_ISDN_X_DMAPTR )
#define	XINE_ISDN_X_NEXTPTR	( XINE_SYS_ASIC + ASIC_ISDN_X_NEXTPTR )
#define	XINE_ISDN_R_DMAPTR	( XINE_SYS_ASIC + ASIC_ISDN_R_DMAPTR )
#define	XINE_ISDN_R_NEXTPTR	( XINE_SYS_ASIC + ASIC_ISDN_R_NEXTPTR )
#define	XINE_REG_CSR		( XINE_SYS_ASIC + ASIC_CSR )
#define	XINE_REG_INTR		( XINE_SYS_ASIC + ASIC_INTR )
#define	XINE_REG_IMSK		( XINE_SYS_ASIC + ASIC_IMSK )
#define	XINE_REG_CURADDR	( XINE_SYS_ASIC + ASIC_CURADDR )
#define	XINE_ISDN_X_DATA	( XINE_SYS_ASIC + ASIC_ISDN_X_DATA )
#define	XINE_ISDN_R_DATA	( XINE_SYS_ASIC + ASIC_ISDN_R_DATA )

#define	XINE_REG_LANCE_DECODE	( XINE_SYS_ASIC + ASIC_LANCE_DECODE )
#define	XINE_REG_SCSI_DECODE	( XINE_SYS_ASIC + ASIC_SCSI_DECODE )
#define	XINE_REG_SCC0_DECODE	( XINE_SYS_ASIC + ASIC_SCC0_DECODE )
#define	XINE_REG_DTOP_DECODE	( XINE_SYS_ASIC + ASIC_SCC1_DECODE )
#define	XINE_REG_FLOPPY_DECODE	( XINE_SYS_ASIC + ASIC_FLOPPY_DECODE )
#	define XINE_LANCE_CONFIG	3
#	define XINE_SCSI_CONFIG		14
#	define XINE_SCC0_CONFIG		(0x10|4)
#	define XINE_DTOP_CONFIG		10
#	define XINE_FLOPPY_CONFIG	13

#define	XINE_REG_SCSI_SCR	( XINE_SYS_ASIC + ASIC_SCSI_SCR )
#define	XINE_REG_SCSI_SDR0	( XINE_SYS_ASIC + ASIC_SCSI_SDR0 )
#define	XINE_REG_SCSI_SDR1	( XINE_SYS_ASIC + ASIC_SCSI_SDR1 )

/*
 *  System registers defines (MREG and CREG)
 */

/* Memory error register */

#define	XINE_MER_xxx		0xf7fe30ff	/* undefined */
#define	XINE_MER_10_1_MS_IP	0x08000000	/* rw: Periodic interrupt */
#define	XINE_MER_PAGE_BRY	0x00010000	/* rw: Page boundary error */
#define	XINE_MER_TLEN		0x00008000	/* rw: Xfer length error */
#define	XINE_MER_PARDIS		0x00004000	/* rw: Dis parity err intr */
#define	XINE_MER_LASTBYTE	0x00000f00	/* rz: Last byte in error: */
#	define	XINE_LASTB31	0x00000800	/* upper byte of word */
#	define	XINE_LASTB23	0x00000400	/* .. through .. */
#	define	XINE_LASTB15	0x00000200	/* .. the .. */
#	define	XINE_LASTB07	0x00000100	/* .. lower byte */

/* Memory size register */

#define	XINE_MSR_xxx		0xffffdfff	/* undefined */
#define	XINE_MSR_10_1_MS_EN	0x04000000	/* rw: enable periodic intr */
#define	XINE_MSR_10_1_MS	0x02000000	/* rw: intr. freq. (0->1ms) */
#define	XINE_MSR_PFORCE		0x01e00000	/* rw: force parity errors */
#define	XINE_MSR_MABEN		0x00100000	/* rw: VRAM ignores SIZE */
#define	XINE_MSR_LAST_BANK	0x000e0000	/* rw: map baseboard mem */
#	define	XINE_BANK_0	0x00020000	/* .. at bank 0, .. */
#	define	XINE_BANK_1	0x00040000	/* .. at bank 1, .. */
#	define	XINE_BANK_2	0x00080000	/* .. or at bank 2  */
#define	XINE_MSR_SIZE_16Mb	0x00002000	/* rw: using 16Mb mem banks */

/* FI register */

#define	XINE_FI_VALUE		0x00001000

/* NOTES

   Memory access priority is, from higher to lower:
	- VRAM/DRAM refresh
	- IO DMA (IO Control ASIC)
	- Slot 0 DMA
	- Processor
	- Slot 1 DMA

   Memory performance is (with 80ns mem cycles)
	- single word read	 5 cyc		10.0 Mb/s
	- word write		 3 cyc		16.7 Mb/s
	- single byte write	 3 cyc		 4.2 Mb/s
	- 64w DMA read		68 cyc		47.1 Mb/s
	- 64w DMA write		66 cyc		48.5 Mb/s
	- Refresh		 5 cyc		N/A
 */

/* Timeout config register */

#define	XINE_CNFG_VALUE		121

/* Address error register */

#define	XINE_AER_ADDR_MASK	0x1ffffffc	/* ro: phys addr in error */

/* Memory access timeout interrupt register */

#define	XINE_TIMEO_INTR		0x00000001	/* rc: intr pending */

/*
 * More system registers defines (IO Control ASIC)
 */

/* (re)defines for the system Status and Control register (SSR) */

#define XINE_CSR_DMAEN_T1	ASIC_CSR_DMAEN_T1
#define XINE_CSR_DMAEN_R1	ASIC_CSR_DMAEN_R1
#define XINE_CSR_DMAEN_DTOP_T	ASIC_CSR_DMAEN_T2
#define XINE_CSR_DMAEN_DTOP_R	ASIC_CSR_DMAEN_R2
#define XINE_CSR_FLOPPY_DIR	ASIC_CSR_FLOPPY_DIR
#define XINE_CSR_DMAEN_FLOPPY	ASIC_CSR_DMAEN_FLOPPY
#define XINE_CSR_DMAEN_ISDN_T	ASIC_CSR_DMAEN_ISDN_T
#define XINE_CSR_DMAEN_ISDN_R	ASIC_CSR_DMAEN_ISDN_R
#define XINE_CSR_SCSI_DIR	ASIC_CSR_SCSI_DIR
#define XINE_CSR_DMAEN_SCSI	ASIC_CSR_DMAEN_SCSI
#define XINE_CSR_DMAEN_LANCE	ASIC_CSR_DMAEN_LANCE
#define XINE_CSR_DIAGDN		0x00008000	/* rw */
#define XINE_CSR_ISDN_ENABLE	0x00001000	/* rw */
#define XINE_CSR_SCC_ENABLE	0x00000800	/* rw */
#define XINE_CSR_RTC_ENABLE	0x00000400	/* rw */
#define XINE_CSR_SCSI_ENABLE	0x00000200	/* rw */
#define XINE_CSR_LANCE_ENABLE	0x00000100	/* rw */
#define XINE_CSR_FLOPPY_ENABLE	0x00000080	/* rw */
#define XINE_CSR_VDAC_ENABLE	0x00000040	/* rw */
#define XINE_CSR_DTOP_ENABLE	0x00000020	/* rw */
#define XINE_CSR_LED		0x00000001	/* rw */

/* (re)defines for the System Interrupt and Mask Registers */

#define	XINE_INTR_T1_PAGE_END	ASIC_INTR_T1_PAGE_END
#define	XINE_INTR_T1_READ_E	ASIC_INTR_T1_READ_E
#define	XINE_INTR_R1_HALF_PAGE	ASIC_INTR_R1_HALF_PAGE
#define	XINE_INTR_R1_DMA_OVRUN	ASIC_INTR_R1_DMA_OVRUN
#define	XINE_INTR_DT_PAGE_END	ASIC_INTR_T2_PAGE_END
#define	XINE_INTR_DT_READ_E	ASIC_INTR_T2_READ_E
#define	XINE_INTR_DT_HALF_PAGE	ASIC_INTR_R2_HALF_PAGE
#define	XINE_INTR_DT_DMA_OVRUN	ASIC_INTR_R2_DMA_OVRUN
#define	XINE_INTR_FLOPPY_DMA_E	ASIC_INTR_FLOPPY_DMA_E
#define	XINE_INTR_ISDN_PTR_LOAD	ASIC_INTR_ISDN_PTR_LOAD
#define	XINE_INTR_ISDN_OVRUN	ASIC_INTR_ISDN_OVRUN
#define	XINE_INTR_ISDN_READ_E	ASIC_INTR_ISDN_READ_E
#define	XINE_INTR_SCSI_PTR_LOAD	ASIC_INTR_SCSI_PTR_LOAD
#define	XINE_INTR_SCSI_OVRUN	ASIC_INTR_SCSI_OVRUN
#define	XINE_INTR_SCSI_READ_E	ASIC_INTR_SCSI_READ_E
#define	XINE_INTR_LANCE_READ_E	ASIC_INTR_LANCE_READ_E
#define	XINE_INTR_xxxx		0x00002808	/* ro */
#define	XINE_INTR_FLOPPY	0x00008000	/* ro */
#define	XINE_INTR_NVR_JUMPER	0x00004000	/* ro */
#define	XINE_INTR_POWERUP	0x00002000	/* ro */
#define	XINE_INTR_TC_0		0x00001000	/* ro */
#define	XINE_INTR_ISDN		0x00000800	/* ro */
#define	XINE_INTR_NRMOD_JUMPER	0x00000400	/* ro */
#define	XINE_INTR_SCSI		0x00000200	/* ro */
#define	XINE_INTR_LANCE		0x00000100	/* ro */
#define	XINE_INTR_FLOPPY_HDS	0x00000080	/* ro */
#define	XINE_INTR_SCC_0		0x00000040	/* ro */
#define	XINE_INTR_TC_1		0x00000020	/* ro */
#define	XINE_INTR_FLOPPY_XDS	0x00000010	/* ro */
#define	XINE_INTR_VINT		0x00000008	/* ro */
#define	XINE_INTR_N_VINT	0x00000004	/* ro */
#define	XINE_INTR_DTOP_TX	0x00000002	/* ro */
#define	XINE_INTR_DTOP_RX	0x00000001	/* ro */
#define	XINE_INTR_ASIC		0xffff0000
#define	XINE_INTR_DTOP		0x00000003
#define	XINE_IM0		0xffff9b6b	/* all good ones enabled */

#endif	/* MIPS_XINE_H */
