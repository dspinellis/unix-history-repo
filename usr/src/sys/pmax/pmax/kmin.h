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
 *	@(#)kmin.h	7.1 (Berkeley) %G%
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
 * $Log:	kmin.h,v $
 * Revision 2.3  92/03/02  18:33:43  rpd
 * 	Split out the ASIC defns into separate file, which is
 * 	in common with MAXine.  Added some nitwits defines.
 * 	[92/03/02  02:28:27  af]
 * 
 * Revision 2.2  91/08/24  12:21:08  af
 * 	Documented new SCSI registers, which were missing in the 3min prototype.
 * 	[91/08/22  11:14:57  af]
 * 
 * 	Created, from the DEC specs:
 * 	"3MIN System Module Functional Specification"  Revision 1.7
 * 	Workstation Systems Engineering, Palo Alto, CA. Sept 14, 1990.
 * 	"KN02BA Daughter Card Functional Specification" Revision 1.0
 * 	Workstation Systems Engineering, Palo Alto, CA. Aug  14, 1990.
 * 	[91/06/21            af]
 * 
 */
/*
 *	File: kmin.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	6/91
 *
 *	Definitions specific to the KN02BA/KN02DA processors and 3MIN
 *	system module (54-20604-01)
 */

#ifndef	MIPS_KMIN_H
#define	MIPS_KMIN_H 1

/*
 * 3MIN's Physical address space
 */

#define KMIN_PHYS_MIN		0x00000000	/* 512 Meg */
#define KMIN_PHYS_MAX		0x1fffffff

/*
 * Memory map
 */

#define KMIN_PHYS_MEMORY_START	0x00000000
#define KMIN_PHYS_MEMORY_END	0x07ffffff	/* 128 Meg in 8 slots */

/*
 * I/O map
 */

#define	KMIN_PHYS_RESERVED	0x08000000	/* Reserved */
						/*  64 Meg */

#define	KMIN_PHYS_MREGS_START	0x0c000000	/* Memory control registers */
#define	KMIN_PHYS_MREGS_END	0x0dffffff	/*  32 Meg */
#define	KMIN_PHYS_CREGS_START	0x0e000000	/* CPU ASIC control regs */
#define	KMIN_PHYS_CREGS_END	0x0fffffff	/*  32 Meg */

#define KMIN_PHYS_TC_0_START	0x10000000	/* TURBOchannel, slot 0 */
#define KMIN_PHYS_TC_0_END	0x13ffffff	/*  64 Meg, option0 */

#define KMIN_PHYS_TC_1_START	0x14000000	/* TURBOchannel, slot 1 */
#define KMIN_PHYS_TC_1_END	0x17ffffff	/*  64 Meg, option1 */

#define KMIN_PHYS_TC_2_START	0x18000000	/* TURBOchannel, slot 2 */
#define KMIN_PHYS_TC_2_END	0x1bffffff	/*  64 Meg, option2 */

#define KMIN_PHYS_TC_3_START	0x1c000000	/* TURBOchannel, slot 3 */
#define KMIN_PHYS_TC_3_END	0x1fffffff	/*  64 Meg, system devices */

#define	KMIN_PHYS_TC_START	KMIN_PHYS_TC_0_START
#define	KMIN_PHYS_TC_END	KMIN_PHYS_TC_3_END	/* 256 Meg */

#define KMIN_TC_NSLOTS		4
#define	KMIN_TC_MIN		0
#define KMIN_TC_MAX		2		/* don't look at system slot */

/* Pseudo-TCslots */
#define	KMIN_SCSI_SLOT		3
#define	KMIN_LANCE_SLOT		4
#define	KMIN_SCC0_SLOT		5
#define	KMIN_SCC1_SLOT		6
#define	KMIN_ASIC_SLOT		7

/*
 * System module space (IO ASIC)
 */

#define	KMIN_SYS_ASIC		( KMIN_PHYS_TC_3_START + 0x0000000 )

#define	KMIN_SYS_ROM_START	( KMIN_SYS_ASIC + ASIC_SLOT_0_START )

#define KMIN_SYS_ASIC_REGS	( KMIN_SYS_ASIC + ASIC_SLOT_1_START )

#define	KMIN_SYS_ETHER_ADDRESS	( KMIN_SYS_ASIC + ASIC_SLOT_2_START )

#define	KMIN_SYS_LANCE		( KMIN_SYS_ASIC + ASIC_SLOT_3_START )

#define	KMIN_SYS_SCC_0		( KMIN_SYS_ASIC + ASIC_SLOT_4_START )

#define	KMIN_SYS_SCC_1		( KMIN_SYS_ASIC + ASIC_SLOT_6_START )

#define	KMIN_SYS_CLOCK		( KMIN_SYS_ASIC + ASIC_SLOT_8_START )

#define	KMIN_SYS_SCSI		( KMIN_SYS_ASIC + ASIC_SLOT_12_START )

#define	KMIN_SYS_SCSI_DMA	( KMIN_SYS_ASIC + ASIC_SLOT_14_START )

#define	KMIN_SYS_BOOT_ROM_START	( KMIN_PHYS_TC_3_START + 0x3c00000 )
#define	KMIN_SYS_BOOT_ROM_END	( KMIN_PHYS_TC_3_START + 0x3c40000 )

/*
 * Interrupts
 */

#define KMIN_INT_FPA		IP_LEV7		/* Floating Point coproc */
#define KMIN_INT_HALTB		IP_LEV6		/* Halt button */
#define KMIN_INT_TC3		IP_LEV5		/* TC slot 3, system */
#define KMIN_INT_TC2		IP_LEV4		/* TC option slot 2 */
#define KMIN_INT_TC1		IP_LEV3		/* TC option slot 1 */
#define KMIN_INT_TC0		IP_LEV2		/* TC option slot 0 */

/*
 *  System registers addresses (MREG and CREG space, and IO Control ASIC)
 */

#define	KMIN_REG_MER		0x0c400000	/* Memory error register */
#define	KMIN_REG_MSR		0x0c800000	/* Memory size register */

#define	KMIN_REG_CNFG		0x0e000000	/* Config mem timeouts */
#define	KMIN_REG_AER		0x0e000004	/* Address error register */
#define	KMIN_REG_BOOT		0x0e000008	/* Boot 0 register */
#define	KMIN_REG_TIMEOUT	0x0e00000c	/* Mem access timeout reg */

#define	KMIN_REG_SCSI_DMAPTR	( KMIN_SYS_ASIC + ASIC_SCSI_DMAPTR )
#define	KMIN_REG_SCSI_DMANPTR	( KMIN_SYS_ASIC + ASIC_SCSI_NEXTPTR )
#define	KMIN_REG_LANCE_DMAPTR	( KMIN_SYS_ASIC + ASIC_LANCE_DMAPTR )
#define	KMIN_REG_SCC_T1_DMAPTR	( KMIN_SYS_ASIC + ASIC_SCC_T1_DMAPTR )
#define	KMIN_REG_SCC_R1_DMAPTR	( KMIN_SYS_ASIC + ASIC_SCC_R1_DMAPTR )
#define	KMIN_REG_SCC_T2_DMAPTR	( KMIN_SYS_ASIC + ASIC_SCC_T2_DMAPTR )
#define	KMIN_REG_SCC_R2_DMAPTR	( KMIN_SYS_ASIC + ASIC_SCC_R2_DMAPTR )
#define	KMIN_REG_CSR		( KMIN_SYS_ASIC + ASIC_CSR )
#define	KMIN_REG_INTR		( KMIN_SYS_ASIC + ASIC_INTR )
#define	KMIN_REG_IMSK		( KMIN_SYS_ASIC + ASIC_IMSK )
#define	KMIN_REG_CURADDR	( KMIN_SYS_ASIC + ASIC_CURADDR )

#define	KMIN_REG_LANCE_DECODE	( KMIN_SYS_ASIC + ASIC_LANCE_DECODE )
#define	KMIN_REG_SCSI_DECODE	( KMIN_SYS_ASIC + ASIC_SCSI_DECODE )
#define	KMIN_REG_SCC0_DECODE	( KMIN_SYS_ASIC + ASIC_SCC0_DECODE )
#define	KMIN_REG_SCC1_DECODE	( KMIN_SYS_ASIC + ASIC_SCC1_DECODE )
#	define KMIN_LANCE_CONFIG	3
#	define KMIN_SCSI_CONFIG		14
#	define KMIN_SCC0_CONFIG		(0x10|4)
#	define KMIN_SCC1_CONFIG		(0x10|6)

#define	KMIN_REG_SCSI_SCR	( KMIN_SYS_ASIC + ASIC_SCSI_SCR )
#define	KMIN_REG_SCSI_SDR0	( KMIN_SYS_ASIC + ASIC_SCSI_SDR0 )
#define	KMIN_REG_SCSI_SDR1	( KMIN_SYS_ASIC + ASIC_SCSI_SDR1 )


/*
 *  System registers defines (MREG and CREG)
 */

/* Memory error register */

#define	KMIN_MER_xxx		0xfffe30ff	/* undefined */
#define	KMIN_MER_PAGE_BRY	0x00010000	/* rw: Page boundary error */
#define	KMIN_MER_TLEN		0x00008000	/* rw: Xfer length error */
#define	KMIN_MER_PARDIS		0x00004000	/* rw: Dis parity err intr */
#define	KMIN_MER_LASTBYTE	0x00000f00	/* rz: Last byte in error: */
#	define	KMIN_LASTB31	0x00000800	/* upper byte of word */
#	define	KMIN_LASTB23	0x00000400	/* .. through .. */
#	define	KMIN_LASTB15	0x00000200	/* .. the .. */
#	define	KMIN_LASTB07	0x00000100	/* .. lower byte */

/* Memory size register */

#define	KMIN_MSR_SIZE_16Mb	0x00002000	/* rw: using 16Mb mem banks */
#define	KMIN_MSR_xxx		0xffffdfff	/* undefined */

/* NOTES

   Memory access priority is, from higher to lower:
	- DRAM refresh
	- IO DMA (IO Control ASIC)
	- Processor
	- Slot 2 DMA
	- Slot 1 DMA
	- Slot 0 DMA

   Memory performance is (with 80ns mem cycles)
	- single word read	 5 cyc		10.0 Mb/s
	- word write		 3 cyc		16.7 Mb/s
	- single byte write	 3 cyc		 4.2 Mb/s
	- 64w DMA read		68 cyc		47.1 Mb/s
	- 64w DMA write		66 cyc		48.5 Mb/s
	- Refresh		 5 cyc		N/A
 */

/* Timeout config register */

#define	KMIN_CNFG_VALUE_12Mhz		127
#define	KMIN_CNFG_VALUE_25Mhz		0

/* Address error register */

#define	KMIN_AER_ADDR_MASK	0x1ffffffc	/* ro: phys addr in error */

/* Boot 0 register */

#define	KMIN_BOOT_FROM_SLOT0	0x00000001	/* rw: diag board boot */

/* Memory access timeout interrupt register */

#define	KMIN_TIMEO_INTR		0x00000001	/* rc: intr pending */

/*
 * More system registers defines (IO Control ASIC)
 */

/* (re)defines for the system Status and Control register (SSR) */

#define KMIN_CSR_DMAEN_T1	ASIC_CSR_DMAEN_T1
#define KMIN_CSR_DMAEN_R1	ASIC_CSR_DMAEN_R1
#define KMIN_CSR_DMAEN_T2	ASIC_CSR_DMAEN_T2
#define KMIN_CSR_DMAEN_R2	ASIC_CSR_DMAEN_R2
#define KMIN_CSR_SCSI_DIR	ASIC_CSR_SCSI_DIR
#define KMIN_CSR_DMAEN_SCSI	ASIC_CSR_DMAEN_SCSI
#define KMIN_CSR_DMAEN_LANCE	ASIC_CSR_DMAEN_LANCE
#define KMIN_CSR_DIAGDN		0x00008000	/* rw */
#define KMIN_CSR_TXDIS_2	0x00004000	/* rw */
#define KMIN_CSR_TXDIS_1	0x00002000	/* rw */
#define KMIN_CSR_SCC_ENABLE	0x00000800	/* rw */
#define KMIN_CSR_RTC_ENABLE	0x00000400	/* rw */
#define KMIN_CSR_SCSI_ENABLE	0x00000200	/* rw */
#define KMIN_CSR_LANCE_ENABLE	0x00000100	/* rw */
#define KMIN_CSR_LEDS		0x000000ff	/* rw */

/* (re)defines for the System Interrupt and Mask Registers */

#define	KMIN_INTR_T1_PAGE_END	ASIC_INTR_T1_PAGE_END
#define	KMIN_INTR_T1_READ_E	ASIC_INTR_T1_READ_E
#define	KMIN_INTR_R1_HALF_PAGE	ASIC_INTR_R1_HALF_PAGE
#define	KMIN_INTR_R1_DMA_OVRUN	ASIC_INTR_R1_DMA_OVRUN
#define	KMIN_INTR_T2_PAGE_END	ASIC_INTR_T2_PAGE_END
#define	KMIN_INTR_T2_READ_E	ASIC_INTR_T2_READ_E
#define	KMIN_INTR_R2_HALF_PAGE	ASIC_INTR_R2_HALF_PAGE
#define	KMIN_INTR_R2_DMA_OVRUN	ASIC_INTR_R2_DMA_OVRUN
#define	KMIN_INTR_SCSI_PTR_LOAD	ASIC_INTR_SCSI_PTR_LOAD
#define	KMIN_INTR_SCSI_OVRUN	ASIC_INTR_SCSI_OVRUN
#define	KMIN_INTR_SCSI_READ_E	ASIC_INTR_SCSI_READ_E
#define	KMIN_INTR_LANCE_READ_E	ASIC_INTR_LANCE_READ_E
#define	KMIN_INTR_NVR_JUMPER	0x00004000	/* ro */
#define	KMIN_INTR_TIMEOUT	0x00001000	/* ro */
#define	KMIN_INTR_NRMOD_JUMPER	0x00000400	/* ro */
#define	KMIN_INTR_SCSI		0x00000200	/* ro */
#define	KMIN_INTR_LANCE		0x00000100	/* ro */
#define	KMIN_INTR_SCC_1		0x00000080	/* ro */
#define	KMIN_INTR_SCC_0		0x00000040	/* ro */
#define	KMIN_INTR_CLOCK		0x00000020	/* ro */
#define	KMIN_INTR_PSWARN	0x00000010	/* ro */
#define	KMIN_INTR_SCSI_FIFO	0x00000004	/* ro */
#define	KMIN_INTR_PBNC		0x00000002	/* ro */
#define	KMIN_INTR_PBNO		0x00000001	/* ro */
#define	KMIN_INTR_ASIC		0xff0f0004
#define	KMIN_IM0		0xff0f13f0	/* all good ones enabled */

#endif	/* MIPS_KMIN_H */
