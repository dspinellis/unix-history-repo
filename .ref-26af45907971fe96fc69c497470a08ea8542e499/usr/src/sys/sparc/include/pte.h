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
 *	@(#)pte.h	7.3 (Berkeley) %G%
 *
 * from: $Header: pte.h,v 1.5 92/11/26 02:04:43 torek Exp $
 */

/*
 * Sun-4 (sort of) and 4c (SparcStation) Page Table Entries
 * (Sun call them `Page Map Entries').
 */

#ifndef LOCORE
/*
 * Segment maps contain `pmeg' (Page Map Entry Group) numbers.
 * A PMEG is simply an index that names a group of 32 (sun4) or
 * 64 (sun4c) PTEs.
 */
#ifdef SUN4
typedef u_short pmeg_t;		/* 9 bits needed per Sun-4 segmap entry */
#else
typedef u_char pmeg_t;		/* 7 bits needed per Sun-4c segmap entry */
#endif
#endif

/*
 * Address translation works as follows:
 *
 *	1. test va<31:29> -- these must be 000 or 111 (or you get a fault)
 *	2. concatenate context_reg<2:0> and va<29:18> to get a 15 bit number;
 *	   use this to index the segment maps, yeilding a 7 or 9 bit value.
 * (for sun4c)
 *	3. take the value from (2) above and concatenate va<17:12> to
 *	   get a `page map entry' index.  This gives a 32-bit PTE.
 * (for sun4)
 *	3. take the value from (2) above and concatenate va<17:13> to
 *	   get a `page map entry' index.  This gives a 32-bit PTE.
 *
 * In other words:
 *
 *	struct sun4_virtual_addr {
 *		u_int	:2,		(required to be the same as bit 29)
 *			va_seg:12,	(virtual segment)
 *			va_pg:5,	(virtual page within segment)
 *			va_off:13;	(offset within page)
 *	};
 *	struct sun4c_virtual_addr {
 *		u_int	:2,		(required to be the same as bit 29)
 *			va_seg:12,	(virtual segment)
 *			va_pg:6,	(virtual page within segment)
 *			va_off:12;	(offset within page)
 *	};
 *
 * Then, given any `va':
 *
 *	extern pmeg_t segmap[8][1<<12];		([16][1<<12] for sun4)
 *	extern int ptetable[128][1<<6];		([512][1<<5] for sun4)
 *
 * (the above being in the hardware, accessed as Alternate Address Spaces)
 *
 *	physseg = segmap[curr_ctx][va.va_seg];
 *	pte = ptetable[physseg][va.va_pg];
 *	if (!(pte & PG_V)) TRAP();
 *	if (writing && !pte.pg_w) TRAP();
 *	if (usermode && pte.pg_s) TRAP();
 *	if (pte & PG_NC) DO_NOT_USE_CACHE_FOR_THIS_ACCESS();
 *	pte |= PG_U;					(mark used/accessed)
 *	if (writing) pte |= PG_M;			(mark modified)
 *	ptetable[physseg][va.va_pg] = pte;
 *	physadr = ((pte & PG_PFNUM) << PGSHIFT) | va.va_off;
 */

#define	NBPSG	(1 << 18)	/* bytes per segment */
#define	SGSHIFT	18		/* log2(NBPSG) */
#define	SGOFSET	(NBPSG - 1)	/* mask for segment offset */

/* number of PTEs that map one segment (not number that fit in one segment!) */
#if defined(SUN4) && defined(SUN4C)
#define	NPTESG	nptesg		/* (which someone will have to init) */
#else
#define	NPTESG	(NBPSG / NBPG)
#endif

/* virtual address to virtual segment number */
#define	VA_VSEG(va)	(((int)(va) >> SGSHIFT) & 0xfff)

/* virtual address to virtual page number, for Sun-4 and Sun-4c */
#define	VA_SUN4_VPG(va)		(((int)(va) >> 13) & 31)
#define	VA_SUN4C_VPG(va)	(((int)(va) >> 12) & 63)

/* truncate virtual address to segment base */
#define	VA_ROUNDDOWNTOSEG(va)	((int)(va) & ~SGOFSET)

/* virtual segment to virtual address (must sign extend!) */
#define	VSTOVA(vseg)	(((int)(vseg) << 20) >> 2)

#ifdef SUN4
#ifdef SUN4C
int	issun4c;
#define VA_VPG(va)	(issun4c ? VA_SUN4C_VPG(va) : VA_SUN4_VPG(va))
#else /* sun4 and not sun4c */
#define VA_VPG(va)	VA_SUN4_VPG(va)
#endif
#else /* not sun4; must be 4c */
#define	VA_VPG(va)	VA_SUN4C_VPG(va)
#endif

/* there is no `struct pte'; we just use `int' */
#define	PG_V		0x80000000
#define	PG_PROT		0x60000000	/* both protection bits */
#define	PG_W		0x40000000	/* allowed to write */
#define	PG_S		0x20000000	/* supervisor only */
#define	PG_NC		0x10000000	/* non-cacheable */
#define	PG_TYPE		0x0c000000	/* both type bits */

#define	PG_OBMEM	0x00000000	/* on board memory */
#define	PG_OBIO		0x04000000	/* on board I/O (incl. Sbus on 4c) */
#ifdef SUN4
#define	PG_VME16	0x08000000	/* 16-bit-data VME space */
#define	PG_VME32	0x0c000000	/* 32-bit-data VME space */
#endif

#define	PG_U		0x02000000
#define	PG_M		0x01000000
#define	PG_MBZ		0x00f80000	/* unused; must be zero (oh really?) */
#define	PG_PFNUM	0x0007ffff	/* n.b.: only 16 bits on sun4c */

#define	PG_TNC_SHIFT	26		/* shift to get PG_TYPE + PG_NC */
#define	PG_M_SHIFT	24		/* shift to get PG_M, PG_U */

/*efine	PG_NOACC	0		** XXX */
#define	PG_KR		0x20000000
#define	PG_KW		0x60000000
#define	PG_URKR		0
#define	PG_UW		0x40000000

#ifdef KGDB
/* but we will define one for gdb anyway */
struct pte {
	u_int	pg_v:1,
		pg_w:1,
		pg_s:1,
		pg_nc:1;
	enum pgtype { pg_obmem, pg_obio, pg_vme16, pg_vme32 } pg_type:2;
	u_int	pg_u:1,
		pg_m:1,
		pg_mbz:5,
		pg_pfnum:19;
};
#endif

/*
 * These are needed in the register window code
 * to check the validity of (ostensible) user stack PTEs.
 */
#define	PG_VSHIFT	30		/* (va>>vshift)==0 or -1 => valid */
	/* XXX fix this name, it is a va shift not a pte bit shift! */

#define	PG_PROTSHIFT	29
#define	PG_PROTUWRITE	6		/* PG_V,PG_W,!PG_S */
#define	PG_PROTUREAD	4		/* PG_V,!PG_W,!PG_S */

/* static __inline int PG_VALID(void *va) {
	register int t = va; t >>= PG_VSHIFT; return (t == 0 || t == -1);
} */
