/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mt. Xinu.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ka650.h	7.5 (Berkeley) %G%
 */

/*
 *
 * Definitions specific to the ka650 (uVAX 3600/3602) cpu card.
 */

#ifdef VAX650
/*
 * CAER: Memory System Error Register (IPR 39)
 */
#define CAER_DAL	0x00000040	/* CDAL or level 2 cache data parity */
#define CAER_MCD	0x00000020	/* mcheck due to DAL parity error */
#define CAER_MCC	0x00000010	/* mcheck due to 1st lev cache parity */
#define CAER_DAT	0x00000002	/* data parity in 1st level cache */
#define CAER_TAG	0x00000001	/* tag parity in 1st level cache */

/*
 * CADR: Cache Disable Register (IPR 37)
 */
#define CADR_STMASK	0x000000f0	/* 1st level cache state mask */
#define CADR_SEN2	0x00000080	/* 1st level cache set 2 enabled */
#define CADR_SEN1	0x00000040	/* 1st level cache set 1 enabled */
#define CADR_CENI	0x00000020	/* 1st level I-stream caching enabled */
#define CADR_CEND	0x00000010	/* 1st level D-stream caching enabled */

/*
 * Internal State Info 2: (for mcheck recovery)
 */
#define IS2_VCR		0x00008000	/* VAX Can't Restart flag */

/*
 * DMA System Error Register (merr_dser)
 */
#define DSER_QNXM	0x00000080	/* Q-22 Bus NXM */
#define DSER_QPE	0x00000020	/* Q-22 Bus parity Error */
#define DSER_MEM	0x00000010	/* Main mem err due to ext dev DMA */
#define DSER_LOST	0x00000008	/* Lost error: DSER <7,5,4,0> set */
#define DSER_NOGRANT	0x00000004	/* No Grant timeout on cpu demand R/W */
#define DSER_DNXM	0x00000001	/* DMA NXM */
#define DSER_CLEAR 	(DSER_QNXM | DSER_QPE | DSER_MEM |  \
			 DSER_LOST | DSER_NOGRANT | DSER_DNXM)
#define DMASER_BITS \
"\20\20BHALT\17DCNEG\10QBNXM\6QBPE\5MEMERR\4LOSTERR\3NOGRANT\1DMANXM"

#ifndef LOCORE
/*
 * Local registers (in I/O space)
 * This is done in disjoint sections.  Map names are set in locore.s
 * and they are mapped in routine configcpu()
 */

/*
 * memory error & configuration registers
 */
struct ka650_merr {
	u_long	merr_scr;	/* System Config Register */
	u_long	merr_dser;	/* DMA System Error Register */
	u_long	merr_qbear;	/* QBus Error Address Register */
	u_long	merr_dear;	/* DMA Error Address Register */
	u_long	merr_qbmbr;	/* Q Bus Map Base address Register */
	u_long	pad[59];
	u_long	merr_csr[16];	/* Main Memory Config Regs (16 banks) */
	u_long	merr_errstat;	/* Main Memory Error Status */
	u_long	merr_cont;	/* Main Memory Control */
};
#define KA650_MERR	0x20080000

/*
 * Main Memory Error Status Register (merr_errstat)
 */
#define MEM_EMASK	0xe0000180	/* mask of all err bits */
#define MEM_RDS		0x80000000	/* uncorrectable main memory */
#define MEM_RDSHIGH	0x40000000	/* high rate RDS errors */
#define MEM_CRD		0x20000000	/* correctable main memory */
#define MEM_DMA		0x00000100	/* DMA read or write error */
#define MEM_CDAL	0x00000080	/* CDAL Parity error on write */
#define MEM_PAGE	0x1ffffe00	/* Offending Page Number */
#define MEM_PAGESHFT	9		/* Shift to normalize page number */

/*
 * Main Memory Control & Diag Status Reg (merr_cont)
 */
#define MEM_CRDINT	0x00001000	/* CRD interrupts enabled */
#define MEM_REFRESH	0x00000800	/* Forced memory refresh */
#define MEM_ERRDIS	0x00000400	/* error detect disable	*/
#define MEM_DIAG	0x00000080	/* Diagnostics mode */
#define MEM_CHECK	0x0000007f	/* check bits for diagnostic mode */

/*
 * Main Memory Config Regs (merr_csr[0-15])
 */
#define MEM_BNKENBLE	0x80000000	/* Bank Enable */
#define MEM_BNKNUM	0x03c00000	/* Physical map Bank number */
#define MEM_BNKUSAGE	0x00000003	/* Bank Usage */

/*
 * Cache Control & Boot/Diag registers
 */
struct ka650_cbd {
	u_char	cbd_cacr;	/* Low byte: Cache Enable & Parity Err detect */
	u_char	cbd_cdf1;	/* Cache diagnostic field (unused) */
	u_char	cbd_cdf2;	/* Cache diagnostic field (unused) */
	u_char	pad;
	u_long	cbd_bdr;	/* Boot & Diagnostic Register (unused) */
};
#define KA650_CBD	0x20084000

/*
 * CACR: Cache Control Register (2nd level cache) (cbd_cacr)
 */
#define CACR_CEN	0x00000010	/* Cache enable */
#define CACR_CPE	0x00000020	/* Cache Parity Error */

/*
 * System Support Chip (SSC) registers
 */
struct ka650_ssc {
	u_long	ssc_sscbr;	/* SSC Base Addr Register */
	u_long	pad1[3];
	u_long	ssc_ssccr;	/* SSC Configuration Register */
	u_long	pad2[3];
	u_long	ssc_cbtcr;	/* CDAL Bus Timeout Control Register */
	u_long	pad3[55];
	u_long	ssc_tcr0;	/* timer control reg 0 */
	u_long	ssc_tir0;	/* timer interval reg 0 */
	u_long	ssc_tnir0;	/* timer next interval reg 0 */
	u_long	ssc_tivr0;	/* timer interrupt vector reg 0 */
	u_long	ssc_tcr1;	/* timer control reg 1 */
	u_long	ssc_tir1;	/* timer interval reg 1 */
	u_long	ssc_tnir1;	/* timer next interval reg 1 */
	u_long	ssc_tivr1;	/* timer interrupt vector reg 1 */
	u_long	pad4[184];
	u_char	ssc_cpmbx;	/* Console Program Mail Box: Lang & Hact */
	u_char	ssc_terminfo;	/* TTY info: Video Dev, MCS, CRT & ROM flags */
	u_char	ssc_keyboard;	/* Keyboard code */
};
#define KA650_SSC	0x20140000

/*
 * CBTCR: CDAL Bus Timeout Control Register (ssc_cbtcr)
 */
#define CBTCR_BTO	0x80000000	/* r/w unimp IPR or unack intr */
#define CBTCR_RWT	0x40000000	/* CDAL Bus Timeout on CPU or DMA */

/*
 * TCR0/TCR1: Programable Timer Control Registers (ssc_tcr[01])
 * (The rest of the bits are the same as in the standard VAX
 *	Interval timer and are defined in clock.h)
 */
#define TCR_STP		0x00000004	/* Stop after time-out */

/*
 * Flags for Console Program Mail Box
 */
#define CPMB650_HALTACT	0x03	/* Field for halt action */
#define CPMB650_RESTART	0x01	/* Restart */
#define CPMB650_REBOOT	0x02	/* Reboot */
#define CPMB650_HALT	0x03	/* Halt */
#define CPMB650_BIP	0x04	/* Bootstrap in progress */
#define CPMB650_RIP	0x08	/* Restart in progress */
#define CPMB650_LANG	0xf0	/* Language field */

/*
 * Inter Processor Communication Register
 * To determine if memory error was from QBUS device DMA (as opposed to cpu).
 */
struct ka650_ipcr {
	u_long	pad[80];
	u_short	ipcr0;		/* InterProcessor Comm Reg for arbiter */
};
#define KA650_IPCR	0x20001e00

#ifndef STANDALONE
/*
 * External declarations of the map names (declared in spt.s)
 * for the local register space.
 */
extern	struct pte KA650MERRmap[];
extern	struct ka650_merr ka650merr;	/* mem err & mem config regs */
extern	struct pte KA650CBDmap[];
extern	struct ka650_cbd ka650cbd;	/* cache control & boot/diag regs */
extern	struct pte KA650SSCmap[];
extern	struct ka650_ssc ka650ssc;	/* SSC regs (& console prog mail box) */
extern	struct pte KA650IPCRmap[];
extern	struct ka650_ipcr ka650ipcr;	/* InterProcessor Com Regs */
extern	struct pte KA650CACHEmap[];
extern	int	ka650cache[];		/* Cache Diagnostic space (for flush) */
#endif	STANDALONE
#endif	LOCORE

/*
 * Physical start address of the Qbus memory.
 * The q-bus memory size is 4 meg.
 * Physical start address of the I/O space (where the 8Kbyte I/O page is).
 */
#define KA650_QMEM	0x30000000
#define KA650_QMEMSIZE	(512*8192)
#define KA650_QDEVADDR	0x20000000

/*
 * Mapping info for Cache Entries, including
 * Size (in bytes) of 2nd Level Cache for cache flush operation
 */
#define KA650_CACHE	0x10000000
#define KA650_CACHESIZE	(64*1024)

/*
 * Useful ROM addresses
 */
#define	KA650ROM_SIDEX	0x20060004	/* system ID extension */
#define	KA650ROM_GETC	0x20060008	/* (jsb) get character from console */
#define	KA650ROM_PUTS	0x2006000c	/* (jsb) put string to console */
#define	KA650ROM_GETS	0x20060010	/* (jsb) read string with prompt */
#define KA650_CONSTYPE	0x20140401	/* byte at which console type resides */
#endif
