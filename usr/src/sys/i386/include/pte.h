/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)pte.h	5.3 (Berkeley) %G%
 */

/*
 * 386 page table entry and page table directory
 * W.Jolitz, 8/89
 *
 * There are two major kinds of pte's: those which have ever existed (and are
 * thus either now in core or on the swap device), and those which have
 * never existed, but which will be filled on demand at first reference.
 * There is a structure describing each.  There is also an ancillary
 * structure used in page clustering.
 */

#ifndef LOCORE
struct pde
{
unsigned int	
		pd_v:1,			/* valid bit */
		pd_prot:2,		/* access control */
		pd_mbz1:2,		/* reserved, must be zero */
		pd_u:1,			/* hardware maintained 'used' bit */
		:1,			/* not used */
		pd_mbz2:2,		/* reserved, must be zero */
		:3,			/* reserved for software */
		pd_pfnum:20;		/* physical page frame number of pte's*/
};
struct pte
{
unsigned int	
		pg_v:1,			/* valid bit */
		pg_prot:2,		/* access control */
		pg_mbz1:2,		/* reserved, must be zero */
		pg_u:1,			/* hardware maintained 'used' bit */
		pg_m:1,			/* hardware maintained modified bit */
		pg_mbz2:2,		/* reserved, must be zero */
		pg_fod:1,		/* is fill on demand (=0) */
		:1,			/* must write back to swap (unused) */
		pg_nc:1,		/* 'uncacheable page' bit */
		pg_pfnum:20;		/* physical page frame number */
};
struct hpte
{
unsigned int	
		pg_high:12,		/* special for clustering */
		pg_pfnum:20;
};
struct fpte
{
unsigned int	
		pg_v:1,			/* valid bit */
		pg_prot:2,		/* access control */
		:5,
		pg_fileno:1,		/* file mapped from or TEXT or ZERO */
		pg_fod:1,		/* is fill on demand (=1) */
		pg_blkno:22;		/* file system block number */
};
#endif

#define	PD_MASK		0xffc00000	/* page directory address bits */
#define	PD_SHIFT	22		/* page directory address bits */

#define	PG_V		0x00000001
#define	PG_PROT		0x00000006 /* all protection bits . */
#define	PG_FOD		0x00000200
#define	PG_SWAPM	0x00000400
#define PG_N		0x00000800 /* Non-cacheable */
#define	PG_M		0x00000040
#define PG_U		0x00000020 /* not currently used */
#define	PG_FRAME	0xfffff000

#define	PG_FZERO	0
#define	PG_FTEXT	1
#define	PG_FMAX		(PG_FTEXT)

#define	PG_NOACC	0
#define	PG_KR		0x00000000
#define	PG_KW		0x00000002
#define	PG_URKR		0x00000004
#define	PG_URKW		0x00000004
#define	PG_UW		0x00000006

/*
 * Page Protection Exception bits
 */

#define PGEX_P		0x01	/* Protection violation vs. not present */
#define PGEX_W		0x02	/* during a Write cycle */
#define PGEX_U		0x04	/* access from User mode (UPL) */

/*
 * Pte related macros
 */
#define	dirty(pte)	((pte)->pg_m)

#ifndef LOCORE
#ifdef KERNEL
/* utilities defined in locore.s */
extern	struct pte Sysmap[];
extern	struct pte Usrptmap[];
extern	struct pte usrpt[];
extern	struct pte Swapmap[];
extern	struct pte Forkmap[];
extern	struct pte Xswapmap[];
extern	struct pte Xswap2map[];
extern	struct pte Pushmap[];
extern	struct pte Vfmap[];
extern	struct pte mmap[];
extern	struct pte msgbufmap[];
extern	struct pte kmempt[], ekmempt[];
#endif
#endif
