/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: cpu.h 1.13 89/06/23$
 *
 *	@(#)cpu.h	7.2 (Berkeley) 5/25/90
 */

/* values for machineid */
#define HP_320		0	/* 16Mhz 68020+HP MMU+16K external cache */
#define HP_330		1	/* 16Mhz 68020+68851 MMU */
#define HP_350		2	/* 25Mhz 68020+HP MMU+32K external cache */
#define HP_360		3	/* 25Mhz 68030 */
#define HP_370		4	/* 33Mhz 68030+64K external cache */
#define HP_340		5	/* 16Mhz 68030 */
#define HP_375		6	/* 50Mhz 68030+32K external cache */

/* values for mmutype (assigned for quick testing) */
#define MMU_68030	-1	/* 68030 on-chip subset of 68851 */
#define MMU_HP		0	/* HP proprietary */
#define MMU_68851	1	/* Motorola 68851 */

/* values for ectype */
#define EC_PHYS		-1	/* external physical address cache */
#define EC_NONE		0	/* no external cache */
#define EC_VIRT		1	/* external virtual address cache */

/* values for cpuspeed (not really related to clock speed due to caches) */
#define MHZ_8		1
#define MHZ_16		2
#define MHZ_25		3
#define MHZ_33		4
#define MHZ_50		6

#ifdef KERNEL
extern	int machineid, mmutype, ectype;
extern	int IObase;

/* what is this supposed to do? i.e. how is it different than startrtclock? */
#define	enablertclock()

#endif

/* physical memory sections */
#define ROMBASE		(0x00000000)
#define IOBASE		(0x00200000)
#define IOTOP		(0x01000000)
#define MAXADDR		(0xFFFFF000)

/* IO space stuff */
#define EXTIOBASE	(0x00600000)
#define	IOCARDSIZE	(0x10000)
#define	IOMAPSIZE	(btoc(IOTOP-IOBASE))
#define	IOP(x)		((x) - IOBASE)
#define	IOV(x)		(((x) - IOBASE) + (int)&IObase)
#define UNIOV(x)	((x) - (int)&IObase + IOBASE)

/* DIO II uncached address space */
#define DIOIIBASE	(0x01000000)
#define DIOIITOP	(0x20000000)
#define DIOIICSIZE	(0x00400000)

/* offsets for longword read/write */
#define	MMUSSTP		IOP(0x5F4000)
#define	MMUUSTP		IOP(0x5F4004)
#define	MMUTBINVAL	IOP(0x5F4008)
#define	MMUSTAT		IOP(0x5F400C)
#define	MMUCMD		MMUSTAT

#define MMU_UMEN	0x0001	/* enable user mapping */
#define MMU_SMEN	0x0002	/* enable supervisor mapping */
#define MMU_CEN		0x0004	/* enable data cache */
#define MMU_BERR	0x0008	/* bus error */
#define MMU_IEN		0x0020	/* enable instruction cache */
#define MMU_FPE		0x0040	/* enable 68881 FP coprocessor */
#define MMU_WPF		0x2000	/* write protect fault */
#define MMU_PF		0x4000	/* page fault */
#define MMU_PTF		0x8000	/* page table fault */

#define MMU_FAULT	(MMU_PTF|MMU_PF|MMU_WPF|MMU_BERR)
#define MMU_ENAB	(MMU_UMEN|MMU_SMEN|MMU_IEN|MMU_FPE)

#define PMMU_LVLMASK	0x0007
#define PMMU_INV	0x0400
#define PMMU_WP		0x0800
#define PMMU_ALV	0x1000
#define PMMU_SO		0x2000
#define PMMU_LV		0x4000
#define PMMU_BE		0x8000

#define PMMU_FAULT	(PMMU_WP|PMMU_INV)

/* function code for user data space */
#define	FC_USERD	1
/* methinks the following is used to selectively clear TLB entries */
#define FC_PURGE	3

/* fields in the 68020 cache control register */
#define IC_ENABLE	0x0001	/* enable instruction cache */
#define IC_FREEZE	0x0002	/* freeze instruction cache */
#define IC_CE		0x0004	/* clear instruction cache entry */
#define IC_CLR		0x0008	/* clear entire instruction cache */

/* additional fields in the 68030 cache control register */
#define IC_BE		0x0010	/* instruction burst enable */
#define DC_ENABLE	0x0100	/* data cache enable */
#define DC_FREEZE	0x0200	/* data cache freeze */
#define DC_CE		0x0400	/* clear data cache entry */
#define DC_CLR		0x0800	/* clear entire data cache */
#define DC_BE		0x1000	/* data burst enable */
#define DC_WA		0x2000	/* write allocate */

#define CACHE_ON	(DC_WA|DC_BE|DC_CLR|DC_ENABLE|IC_BE|IC_CLR|IC_ENABLE)
#define CACHE_OFF	(DC_CLR|IC_CLR)
#define CACHE_CLR	(CACHE_ON)
#define IC_CLEAR	(DC_WA|DC_BE|DC_ENABLE|IC_BE|IC_CLR|IC_ENABLE)
#define DC_CLEAR	(DC_WA|DC_BE|DC_CLR|DC_ENABLE|IC_BE|IC_ENABLE)
