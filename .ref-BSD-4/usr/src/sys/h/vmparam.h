/*	vmparam.h	4.1	11/9/80	*/

/*
 * Machine dependent constants
 */
#define	NBBY		8		/* number of bits in a byte */
#define	NBPG		512		/* number of bytes per page */
#define	PGSHIFT		9		/* LOG2(NBPG) */
#define	NPTEPG		(NBPG/(sizeof (struct pte)))
					/* number of ptes per page */
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	CLOFSET		(CLSIZE*NBPG-1)	/* for clusters, like PGOFSET */
#define	USRSTACK	(0x80000000-UPAGES*NBPG)
					/* Start of user stack */
#define	P1TOP		0x200000	/* boundary between P0 and P1 regions */
#define	AST		0x04000000	/* ast level */

/*
 * Virtual memory related constants
 *
 * note: USRPTSIZE is well known in locore.s
 */
#define	SLOP	16
#define	MAXTSIZ		(6*2048-SLOP)		/* max text size (clicks) */
#define	MAXDSIZ		(11*1024-16-SLOP)	/* max data size (clicks) */
#define	MAXSSIZ		(11*1024-16-SLOP)	/* max stack size (clicks) */
#define	USRPTSIZE 	(8*NPTEPG)	/* max number of pages of page tables
					   for resident processes, this is
					   known in locore.s */
