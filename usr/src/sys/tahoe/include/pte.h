/*	pte.h	1.4	87/06/06	*/

/*
 * Tahoe page table entry
 *
 * There are two major kinds of pte's: those which have ever existed (and are
 * thus either now in core or on the swap device), and those which have
 * never existed, but which will be filled on demand at first reference.
 * There is a structure describing each.  There is also an ancillary
 * structure used in page clustering.
 */

#ifndef LOCORE
struct pte
{
unsigned int	
		pg_v:1,			/* valid bit */
		pg_prot:4,		/* access control */
		pg_fod:1,		/* is fill on demand (=0) */
		:1,			/* must write back to swap (unused) */
		pg_nc:1,		/* 'uncacheable page' bit */
		pg_m:1,			/* hardware maintained modified bit */
		pg_u:1,			/* hardware maintained 'used' bit */
		pg_pfnum:22;		/* core page frame number or 0 */
};
struct hpte
{
unsigned int	
		pg_high:10,		/* special for clustering */
		pg_pfnum:22;
};
struct fpte
{
unsigned int	
		pg_v:1,
		pg_prot:4,
		pg_fod:1,		/* is fill on demand (=1) */
		:1,
		pg_fileno:1,		/* file mapped from or TEXT or ZERO */
		pg_blkno:24;		/* file system block number */
};
#endif

#define	PG_V		0x80000000
#define	PG_PROT		0x78000000 /* all protection bits  (dorit). */
#define	PG_FOD		0x04000000
#define	PG_SWAPM	0x02000000
#define PG_N		0x01000000 /* Non-cacheable */
#define	PG_M		0x00800000
#define PG_U		0x00400000 /* not currently used */
#define	PG_PFNUM	0x003fffff

#define	PG_FZERO	0
#define	PG_FTEXT	1
#define	PG_FMAX		(PG_FTEXT)

#define	PG_NOACC	0
#define	PG_KR		0x40000000
#define	PG_KW		0x60000000
#define	PG_URKR		0x50000000
#define	PG_URKW		0x70000000
#define	PG_UW		0x78000000

/*
 * Pte related macros
 */
#define	dirty(pte)	((pte)->pg_fod == 0 && (pte)->pg_pfnum && (pte)->pg_m)

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
