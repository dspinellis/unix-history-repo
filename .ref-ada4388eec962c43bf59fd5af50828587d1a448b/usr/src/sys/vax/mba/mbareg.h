/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)mbareg.h	7.2 (Berkeley) %G%
 */

/*
 * VAX MASSBUS adapter registers
 */

struct mba_regs
{
	int	mba_csr;		/* configuration register */
	int	mba_cr;			/* control register */
	int	mba_sr;			/* status register */
	int	mba_var;		/* virtual address register */
	int	mba_bcr;		/* byte count register */
	int	mba_dr;
	int	mba_pad1[250];
	struct mba_drv {		/* per drive registers */
		int	mbd_cs1;		/* control status */
		int	mbd_ds;			/* drive status */
		int	mbd_er1;		/* error register */
		int	mbd_mr1;		/* maintenance register */
		int	mbd_as;			/* attention status */
		int	mbd_da;			/* desired address (disks) */
#define	mbd_fc	mbd_da				/* frame count (tapes) */
		int	mbd_dt;			/* drive type */
		int	mbd_la;			/* look ahead (disks) */
#define	mbd_ck	mbd_la				/* ??? (tapes) */
		int	mbd_sn;			/* serial number */
		int	mbd_of;			/* ??? */
#define	mbd_tc	mbd_of				/* ??? */
		int	mbd_fill[22];
	} mba_drv[8];
	struct	pte mba_map[256];	/* io space virtual map */
	int	mba_pad2[256*5];	/* to size of a nexus */
};

/*
 * Bits in mba_cr
 */
#define	MBCR_INIT	0x1		/* init mba */
#define	MBCR_IE		0x4		/* enable mba interrupts */

/*
 * Bits in mba_sr
 */
#define	MBSR_DTBUSY	0x80000000	/* data transfer busy */
#define	MBSR_NRCONF	0x40000000	/* no response confirmation */
#define	MBSR_CRD	0x20000000	/* corrected read data */
#define	MBSR_CBHUNG	0x00800000	/* control bus hung */
#define	MBSR_PGE	0x00080000	/* programming error */
#define	MBSR_NED	0x00040000	/* non-existant drive */
#define	MBSR_MCPE	0x00020000	/* massbus control parity error */
#define	MBSR_ATTN	0x00010000	/* attention from massbus */
#define	MBSR_SPE	0x00004000	/* silo parity error */
#define	MBSR_DTCMP	0x00002000	/* data transfer completed */
#define	MBSR_DTABT	0x00001000	/* data transfer aborted */
#define	MBSR_DLT	0x00000800	/* data late */
#define	MBSR_WCKUP	0x00000400	/* write check upper */
#define	MBSR_WCKLWR	0x00000200	/* write check lower */
#define	MBSR_MXF	0x00000100	/* miss transfer error */
#define	MBSR_MBEXC	0x00000080	/* massbus exception */
#define	MBSR_MDPE	0x00000040	/* massbus data parity error */
#define	MBSR_MAPPE	0x00000020	/* page frame map parity error */
#define	MBSR_INVMAP	0x00000010	/* invalid map */
#define	MBSR_ERRCONF	0x00000008	/* error confirmation */
#define	MBSR_RDS	0x00000004	/* read data substitute */
#define	MBSR_ISTIMO	0x00000002	/* interface sequence timeout */
#define	MBSR_RDTIMO	0x00000001	/* read data timeout */

#define MBSR_BITS \
"\20\40DTBUSY\37NRCONF\36CRD\30CBHUNG\24PGE\23NED\22MCPE\21ATTN\
\17SPE\16DTCMP\15DTABT\14DLT\13WCKUP\12WCKLWR\11MXF\10MBEXC\7MDPE\
\6MAPPE\5INVMAP\4ERRCONF\3RDS\2ISTIMO\1RDTIMO"

#define	MBSR_HARD	(MBSR_PGE|MBSR_ERRCONF|MBSR_ISTIMO|MBSR_RDTIMO)

#define MBSR_EBITS	(~(MBSR_DTBUSY|MBSR_CRD|MBSR_ATTN|MBSR_DTCMP))

#ifdef KERNEL
extern	char	mbsr_bits[];
#endif

/*
 * Commands for mbd_cs1
 */
#define	MB_WCOM		0x30
#define	MB_RCOM		0x38
#define	MB_GO		0x1

/*
 * Bits in mbd_ds.
 */
#define	MBDS_ERR	0x00004000	/* error in drive */
#define	MBDS_MOL	0x00001000	/* medium on line */
#define	MBDS_DPR	0x00000100	/* drive present */
#define	MBDS_DRY	0x00000080	/* drive ready */

#define	MBDS_DREADY	(MBDS_MOL|MBDS_DPR|MBDS_DRY)

/*
 * Bits in mbd_dt
 */
#define	MBDT_NSA	0x8000		/* not sector addressible */
#define	MBDT_TAP	0x4000		/* is a tape */
#define	MBDT_MOH	0x2000		/* moving head */
#define	MBDT_7CH	0x1000		/* 7 channel */
#define	MBDT_DRQ	0x800		/* drive request required */
#define	MBDT_SPR	0x400		/* slave present */

#define	MBDT_TYPE	0x1ff
#define	MBDT_MASK	(MBDT_NSA|MBDT_TAP|MBDT_TYPE)

/* type codes for disk drives */
#define	MBDT_RP04	020
#define	MBDT_RP05	021
#define	MBDT_RP06	022
#define	MBDT_RP07	042
#define	MBDT_RM02	025
#define	MBDT_RM03	024
#define	MBDT_RM05	027
#define	MBDT_RM80	026
#define	MBDT_ML11A	0110
#define	MBDT_ML11B	0111

/* type codes for tape drives */
#define	MBDT_TM03	050
#define	MBDT_TE16	051
#define	MBDT_TU45	052
#define	MBDT_TU77	054
#define	MBDT_TU78	0101
