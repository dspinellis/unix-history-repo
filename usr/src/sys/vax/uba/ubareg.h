/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ubareg.h	7.5 (Berkeley) %G%
 */

/*
 * VAX UNIBUS adapter registers
 */

/*
 * "UNIBUS" adaptor types.
 * This code is used for both UNIBUSes and Q-buses
 * with different types of adaptors.
 * Definition of a type includes support code for that type.
 */
#if VAX780 || VAX8600
#define	DW780	1		/* has adaptor regs, sr: 780/785/8600 */
#endif

#if VAX750
#define	DW750	2		/* has adaptor regs, no sr: 750, 730 */
#endif

#if VAX730
#define	DW730	3		/* has adaptor regs, no sr: 750, 730 */
#endif

#if VAX630 || VAX650
#define	QBA	4		/* 22-bit Q-bus, no adaptor regs: uVAX II */
#endif

#if VAX8200 || VAX8500 || VAX8800
#define	DWBUA	5		/* BI UNIBUS adaptor: 8200/8500/8800 */
#endif

/*
 * Size of unibus memory address space in pages
 * (also number of map registers).
 * QBAPAGES should be 8192, but umem needs to be expanded.
 */
#define	UBAPAGES	496
#define	NUBMREG		496
#define	QBAPAGES	UBAPAGES	/* for now; should be 8192 */
#define	UBAIOADDR	0760000		/* start of I/O page */
#define	UBAIOPAGES	16

#ifndef LOCORE
/*
 * DWBUA hardware registers.
 */
struct dwbua_regs {
	int	pad1[456];		/* actually bii regs + pad */
	int	bua_csr;		/* control and status register */
	int	bua_offset;		/* vector offset register */
	int	bua_fubar;		/* failed UNIBUS address register */
	int	bua_bifar;		/* BI failed address register */
	int	bua_udiag[5];		/* micro diagnostics (R/O) */
	int	pad2[3];
/* dpr[0] is for DDP; dpr's 1 through 5 are for BPD's 1 through 5 */
	int	bua_dpr[6];		/* data path registers */
	int	pad3[10];
	int	bua_bdps[20];		/* buffered data path space *//*???*/
	int	pad4[8];
	struct	pte bua_map[UBAPAGES];	/* unibus map registers */
	int	pad5[UBAIOPAGES];	/* no maps for device address space */
};

#ifdef DWBUA
/* bua_csr */
#define	BUACSR_ERR	0x80000000	/* composite error */
#define	BUACSR_BIF	0x10000000	/* BI failure */
#define	BUACSR_SSYNTO	0x08000000	/* slave sync timeout */
#define	BUACSR_UIE	0x04000000	/* unibus interlock error */
#define	BUACSR_IVMR	0x02000000	/* invalid map register */
#define	BUACSR_BADBDP	0x01000000	/* bad BDP select */
#define	BUACSR_BUAEIE	0x00100000	/* bua error interrupt enable (?) */
#define	BUACSR_UPI	0x00020000	/* unibus power init */
#define	BUACSR_UREGDUMP	0x00010000	/* microdiag register dump */
#define	BUACSR_IERRNO	0x000000ff	/* mask for internal errror number */

/* bua_offset */
#define	BUAOFFSET_MASK	0x00003e00	/* hence max offset = 15872 */

/* bua_dpr */
#define	BUADPR_DPSEL	0x00e00000	/* data path select (?) */
#define	BUADPR_PURGE	0x00000001	/* purge bdp */

/* bua_map -- in particular, those bits that are not in DW780s & DW750s */
#define	BUAMR_IOADR	0x40000000	/* I/O address space */
#define	BUAMR_LAE	0x04000000	/* longword access enable */
	/* I see no reason to use either one, though ... act 6 Aug 1987 */

#define	UBA_PURGEBUA(uba, bdp) \
	(((struct dwbua_regs *)(uba))->bua_dpr[bdp] |= BUADPR_PURGE)
#else
#define	UBA_PURGEBUA(uba, bdp)
#endif

/*
 * DW780/DW750 hardware registers
 */
struct uba_regs {
	int	uba_cnfgr;		/* configuration register */
	int	uba_cr;			/* control register */
	int	uba_sr;			/* status register */
	int	uba_dcr;		/* diagnostic control register */
	int	uba_fmer;		/* failed map entry register */
	int	uba_fubar;		/* failed UNIBUS address register */
	int	pad1[2];
	int	uba_brsvr[4];
	int	uba_brrvr[4];		/* receive vector registers */
	int	uba_dpr[16];		/* buffered data path register */
	int	pad2[480];
	struct	pte uba_map[UBAPAGES];	/* unibus map register */
	int	pad3[UBAIOPAGES];	/* no maps for device address space */
};
#endif

#ifdef DW780
/* uba_cnfgr */
#define	UBACNFGR_UBINIT	0x00040000	/* unibus init asserted */
#define	UBACNFGR_UBPDN	0x00020000	/* unibus power down */
#define	UBACNFGR_UBIC	0x00010000	/* unibus init complete */

#define UBACNFGR_BITS \
"\40\40PARFLT\37WSQFLT\36URDFLT\35ISQFLT\34MXTFLT\33XMTFLT\30ADPDN\27ADPUP\23UBINIT\22UBPDN\21UBIC"

/* uba_cr */
#define	UBACR_MRD16	0x40000000	/* map reg disable bit 4 */
#define	UBACR_MRD8	0x20000000	/* map reg disable bit 3 */
#define	UBACR_MRD4	0x10000000	/* map reg disable bit 2 */
#define	UBACR_MRD2	0x08000000	/* map reg disable bit 1 */
#define	UBACR_MRD1	0x04000000	/* map reg disable bit 0 */
#define	UBACR_IFS	0x00000040	/* interrupt field switch */
#define	UBACR_BRIE	0x00000020	/* BR interrupt enable */
#define	UBACR_USEFIE	0x00000010	/* UNIBUS to SBI error field IE */
#define	UBACR_SUEFIE	0x00000008	/* SBI to UNIBUS error field IE */
#define	UBACR_CNFIE	0x00000004	/* configuration IE */
#define	UBACR_UPF	0x00000002	/* UNIBUS power fail */
#define	UBACR_ADINIT	0x00000001	/* adapter init */

/* uba_sr */
#define	UBASR_BR7FULL	0x08000000	/* BR7 receive vector reg full */
#define	UBASR_BR6FULL	0x04000000	/* BR6 receive vector reg full */
#define	UBASR_BR5FULL	0x02000000	/* BR5 receive vector reg full */
#define	UBASR_BR4FULL	0x01000000	/* BR4 receive vector reg full */
#define	UBASR_RDTO	0x00000400	/* UNIBUS to SBI read data timeout */
#define	UBASR_RDS	0x00000200	/* read data substitute */
#define	UBASR_CRD	0x00000100	/* corrected read data */
#define	UBASR_CXTER	0x00000080	/* command transmit error */
#define	UBASR_CXTMO	0x00000040	/* command transmit timeout */
#define	UBASR_DPPE	0x00000020	/* data path parity error */
#define	UBASR_IVMR	0x00000010	/* invalid map register */
#define	UBASR_MRPF	0x00000008	/* map register parity failure */
#define	UBASR_LEB	0x00000004	/* lost error */
#define	UBASR_UBSTO	0x00000002	/* UNIBUS select timeout */
#define	UBASR_UBSSYNTO	0x00000001	/* UNIBUS slave sync timeout */

#define	UBASR_BITS \
"\20\13RDTO\12RDS\11CRD\10CXTER\7CXTMO\6DPPE\5IVMR\4MRPF\3LEB\2UBSTO\1UBSSYNTO"

/* uba_brrvr[] */
#define	UBABRRVR_AIRI	0x80000000	/* adapter interrupt request */
#define	UBABRRVR_DIV	0x0000ffff	/* device interrupt vector field */
#endif
 
/* uba_dpr */
#ifdef DW780
#define	UBADPR_BNE	0x80000000	/* buffer not empty - purge */
#define	UBADPR_BTE	0x40000000	/* buffer transfer error */
#define	UBADPR_DPF	0x20000000	/* DP function (RO) */
#define	UBADPR_BS	0x007f0000	/* buffer state field */
#define	UBADPR_BUBA	0x0000ffff	/* buffered UNIBUS address */
#define	UBA_PURGE780(uba, bdp) \
    ((uba)->uba_dpr[bdp] |= UBADPR_BNE)
#else
#define UBA_PURGE780(uba, bdp)
#endif
#ifdef DW750
#define	UBADPR_ERROR	0x80000000	/* error occurred */
#define	UBADPR_NXM	0x40000000	/* nxm from memory */
#define	UBADPR_UCE	0x20000000	/* uncorrectable error */
#define	UBADPR_PURGE	0x00000001	/* purge bdp */
/* the DELAY is for a hardware problem */
#define	UBA_PURGE750(uba, bdp) { \
    ((uba)->uba_dpr[bdp] |= (UBADPR_PURGE|UBADPR_NXM|UBADPR_UCE)); \
    DELAY(8); \
}
#else
#define UBA_PURGE750(uba, bdp)
#endif

/*
 * Macros for fast buffered data path purging in time-critical routines.
 *
 * Too bad C pre-processor doesn't have the power of LISP in macro
 * expansion...
 */

/* THIS IS WRONG, should use pointer to uba_hd */
#if DWBUA || DW780 || DW750
#define	UBAPURGE(uba, bdp) { \
	switch (cpu) { \
	case VAX_8200: UBA_PURGEBUA(uba, bdp); break; \
	case VAX_8600: case VAX_780: UBA_PURGE780((uba), (bdp)); break; \
	case VAX_750: UBA_PURGE750((uba), (bdp)); break; \
	} \
}
#else
#define	UBAPURGE(uba, bdp)
#endif



/* uba_mr[] */
#define	UBAMR_MRV	0x80000000	/* map register valid */
#define	UBAMR_BO	0x02000000	/* byte offset bit */
#define	UBAMR_DPDB	0x01e00000	/* data path designator field */
#define	UBAMR_SBIPFN	0x001fffff	/* SBI page address field */

#define	UBAMR_DPSHIFT	21		/* shift to data path designator */

/*
 * Number of unibus buffered data paths and possible uba's per cpu type.
 */
#define	NBDP8600	15
#define	NBDP780		15
#define	NBDPBUA		5
#define	NBDP750		3
#define	NBDP730		0
#define	MAXNBDP		15

/*
 * Symbolic BUS addresses for UBAs.
 */

#if VAX630 || VAX650
#define	QBAMAP630	((struct pte *)0x20088000)
#define	QMEM630		0x30000000
#define	QIOPAGE630	0x20000000
/*
 * Q-bus control registers
 */
#define	QIPCR		0x1f40		/* from start of iopage */
/* bits in QIPCR */
#define	Q_DBIRQ		0x0001		/* doorbell interrupt request */
#define	Q_LMEAE		0x0020		/* local mem external access enable */
#define	Q_DBIIE		0x0040		/* doorbell interrupt enable */
#define	Q_AUXHLT	0x0100		/* auxiliary processor halt */
#define	Q_DMAQPE	0x8000		/* Q22 bus address space parity error */
#endif

#if VAX730
#define	UMEM730		0xfc0000
#endif

#if VAX750
#define	UMEM750(i)	(0xfc0000-(i)*0x40000)
#endif

#if VAX780
#define	UMEM780(i)	(0x20100000+(i)*0x40000)
#endif

#if VAX8200		/* BEWARE, argument is node, not ubanum */
#define	UMEM8200(i)	(0x20400000+(i)*0x40000)
#endif

#if VAX8600
#define	UMEMA8600(i)	(0x20100000+(i)*0x40000)
#define	UMEMB8600(i)	(0x22100000+(i)*0x40000)
#endif

/*
 * Macro to offset a UNIBUS device address, often expressed as
 * something like 0172520, by forcing it into the last 8K
 * of UNIBUS memory space.
 */
#define	ubdevreg(addr)	((addr) & 017777)
