/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)malloc.h	7.3 (Berkeley) %G%
 */

#define KMEMSTATS

/*
 * flags to malloc
 */
#define M_WAITOK	0x0000
#define M_NOWAIT	0x0001

/*
 * Types of memory to be allocated
 */
#define	M_FREE		0	/* should be on free list */
#define M_MBUF		1	/* mbuf */
#define M_DEVBUF	2	/* device driver memory */
#define	M_SOCKET	3	/* socket structure */
#define	M_PCB		4	/* protocol control block */
#define	M_RTABLE	5	/* routing tables */
#define	M_HTABLE	6	/* IMP host tables */
#define	M_FTABLE	7	/* fragment reassembly header */
#define	M_ZOMBIE	8	/* zombie proc status */
#define	M_IFADDR	9	/* interface address */
#define	M_SOOPTS	10	/* socket options */
#define	M_SONAME	11	/* socket name */
#define M_NAMEI		12	/* namei path name buffer */
#define M_GPROF		13	/* kernel profiling buffer */
#define M_IOCTLOPS	14	/* ioctl data buffer */
#define M_SUPERBLK	15	/* super block data */
#define M_TEMP		16	/* misc temporary data buffers */
#define M_LAST		50

struct kmemstats {
	long	ks_inuse;	/* # of packets of this type currently in use */
	long	ks_calls;	/* total packets of this type ever allocated */
	long	ks_maxused;	/* maximum number ever used */
	long	ks_limit;	/* most that are allowed to exist */
};

/*
 * Array of descriptors that describe the contents of each page
 */
struct kmemusage {
	short ku_indx;		/* bucket index */
	union {
		u_short freecnt;/* for small allocations, free pieces in page */
		u_short pagecnt;/* for large allocations, pages alloced */
	} ku_un;
};
#define ku_freecnt ku_un.freecnt
#define ku_pagecnt ku_un.pagecnt

/*
 * Set of buckets for each size of memory block that is retained
 */
struct kmembuckets {
	caddr_t kb_next;	/* list of free blocks */
	long	kb_calls;	/* total calls to allocate this size */
	long	kb_total;	/* total number of blocks allocated */
	long	kb_totalfree;	/* # of free elements in this bucket */
	long	kb_elmpercl;	/* # of elements in this sized allocation */
	long	kb_highwat;	/* high water mark */
	long	kb_couldfree;	/* over high water mark and could free */
};

#ifdef KERNEL
#define MINALLOCSIZE	(1 << MINBUCKET)
#define BUCKETINDX(size) \
	(size) <= (MINALLOCSIZE * 128) \
		? (size) <= (MINALLOCSIZE * 8) \
			? (size) <= (MINALLOCSIZE * 2) \
				? (size) <= (MINALLOCSIZE * 1) \
					? (MINBUCKET + 0) \
					: (MINBUCKET + 1) \
				: (size) <= (MINALLOCSIZE * 4) \
					? (MINBUCKET + 2) \
					: (MINBUCKET + 3) \
			: (size) <= (MINALLOCSIZE* 32) \
				? (size) <= (MINALLOCSIZE * 16) \
					? (MINBUCKET + 4) \
					: (MINBUCKET + 5) \
				: (size) <= (MINALLOCSIZE * 64) \
					? (MINBUCKET + 6) \
					: (MINBUCKET + 7) \
		: (size) <= (MINALLOCSIZE * 2048) \
			? (size) <= (MINALLOCSIZE * 512) \
				? (size) <= (MINALLOCSIZE * 256) \
					? (MINBUCKET + 8) \
					: (MINBUCKET + 9) \
				: (size) <= (MINALLOCSIZE * 1024) \
					? (MINBUCKET + 10) \
					: (MINBUCKET + 11) \
			: (size) <= (MINALLOCSIZE * 8192) \
				? (size) <= (MINALLOCSIZE * 4096) \
					? (MINBUCKET + 12) \
					: (MINBUCKET + 13) \
				: (size) <= (MINALLOCSIZE * 16384) \
					? (MINBUCKET + 14) \
					: (MINBUCKET + 15)

/*
 * Turn virtual addresses into kmem map indicies
 */
#define kmemxtob(addr)	(kmembase + (addr) * NBPG)
#define btokmemx(addr)	(((addr) - kmembase) / NBPG)
#define btokup(addr)	(&kmemusage[((addr) - kmembase) >> CLSHIFT])

/*
 * Macro versions for the usual cases of malloc/free
 */
#ifdef KMEMSTATS
#define MALLOC(space, cast, size, type, flags) \
	(space) = (cast)malloc(size, type, flags)
#define FREE(addr, type) free(addr, type)

#else /* do not collect statistics */
#define MALLOC(space, cast, size, type, flags) { \
	register struct kmembuckets *kbp = &bucket[BUCKETINDX(size)]; \
	long s = splimp(); \
	if (kbp->kb_next == NULL) { \
		(space) = (cast)malloc(size, type, flags); \
	} else { \
		(space) = (cast)kbp->kb_next; \
		kbp->kb_next = *(caddr_t *)(space); \
	} \
	splx(s); \
}

#define FREE(addr, type) { \
	register struct kmembuckets *kbp; \
	register struct kmemusage *kup = btokup(addr); \
	long s = splimp(); \
	if (1 << kup->ku_indx > MAXALLOCSAVE) { \
		free(addr, type); \
	} else { \
		kbp = &bucket[kup->ku_indx]; \
		*(caddr_t *)(addr) = kbp->kb_next; \
		kbp->kb_next = (caddr_t)(addr); \
	} \
	splx(s); \
}
#endif /* do not collect statistics */

extern struct kmemstats kmemstats[];
extern struct kmemusage *kmemusage;
extern char kmembase[];
extern struct kmembuckets bucket[];
extern qaddr_t malloc();
extern void free();
#endif KERNEL
