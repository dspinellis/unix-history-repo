/* 
 * Copyright (c) 1987 Carnegie-Mellon University
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmap.h	7.5 (Berkeley) %G%
 */

#ifndef	_PMAP_MACHINE_
#define	_PMAP_MACHINE_

/*
 * TLB hash table values.
 * SHIFT2 should shift virtual address bit 22 to the high bit of the index.
 *			address:	index:
 *	USRTEXT		0x00400000	10xxxxxxx
 *	USRDATA		0x10000000	00xxxxxxx
 *	USRSTACK	0x7FFFFFFF	11xxxxxxx
 * This gives 1/2 the table to data, 1/4 for text and 1/4 for stack.
 * Note: the current process has its hash table mapped at PMAP_HASH_UADDR.
 *	the kernel's hash table is mapped at PMAP_HASH_KADDR.
 *	The size of the hash table is known in locore.s.
 * The wired entries in the TLB will contain the following:
 *	UPAGES			(for curproc)
 *	PMAP_HASH_UPAGES	(for curproc)
 *	PMAP_HASH_KPAGES	(for kernel)
 * The kernel doesn't actually use a pmap_hash_t, the pm_hash field is NULL and
 * all the PTE entries are stored in a single array at PMAP_HASH_KADDR.
 * If we need more KPAGES that the TLB has wired entries, then we can switch
 * to a global pointer for the kernel TLB table.
 * If we try to use a hash table for the kernel, wired TLB entries become a
 * problem.
 * Note: PMAP_HASH_UPAGES should be a multiple of MACH pages (see pmap_enter()).
 */
#define PMAP_HASH_UPAGES	1
#define PMAP_HASH_KPAGES	4
#define PMAP_HASH_UADDR		(UADDR - PMAP_HASH_UPAGES * NBPG)
#define PMAP_HASH_KADDR		(UADDR - (PMAP_HASH_UPAGES + PMAP_HASH_KPAGES) * NBPG)
#define PMAP_HASH_NUM_ENTRIES	256
#define PMAP_HASH_SIZE_SHIFT	4
#define PMAP_HASH_SHIFT1	12
#define PMAP_HASH_SHIFT2	21
#define PMAP_HASH_MASK1		0x07f
#define PMAP_HASH_MASK2		0x080
#define PMAP_HASH_SIZE		(PMAP_HASH_NUM_ENTRIES*sizeof(struct pmap_hash))

/* compute pointer to pmap hash table */
#define PMAP_HASH(va) \
	((((va) >> PMAP_HASH_SHIFT1) & PMAP_HASH_MASK1) | \
	 (((va) >> PMAP_HASH_SHIFT2) & PMAP_HASH_MASK2))

/*
 * A TLB hash entry.
 */
typedef struct pmap_hash {
	struct {
		u_int	low;		/* The TLB low register value. */
		u_int	high;		/* The TLB high register value. */
	} pmh_pte[2];
} *pmap_hash_t;

/*
 * Machine dependent pmap structure.
 */
typedef struct pmap {
	int			pm_count;	/* pmap reference count */
	simple_lock_data_t	pm_lock;	/* lock on pmap */
	struct pmap_statistics	pm_stats;	/* pmap statistics */
	int			pm_flags;	/* see below */
	int			pm_tlbpid;	/* address space tag */
	pmap_hash_t		pm_hash;	/* TLB cache */
	unsigned		pm_hash_ptes[PMAP_HASH_UPAGES];
} *pmap_t;

#define PM_MODIFIED	1		/* flush tlbpid before resume() */

/*
 * Defines for pmap_attributes[phys_mach_page];
 */
#define PMAP_ATTR_MOD	0x01	/* page has been modified */
#define PMAP_ATTR_REF	0x02	/* page has been referenced */

#ifdef	KERNEL
extern struct pmap kernel_pmap_store;
#define kernel_pmap (&kernel_pmap_store)
extern	char *pmap_attributes;		/* reference and modify bits */
#endif	KERNEL
#endif	_PMAP_MACHINE_
