/* 
 * Copyright (c) 1987 Carnegie-Mellon University
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * The CMU software License Agreement specifies the terms and conditions
 * for use and redistribution.
 *
 *	@(#)pmap.h	7.3 (Berkeley) %G%
 */

#ifndef	_PMAP_MACHINE_
#define	_PMAP_MACHINE_

#define HP_PAGE_SIZE	NBPG
#define HP_SEG_SIZE	NBSEG

/*
 * Pmap stuff
 */
struct pmap {
	struct pte		*pm_ptab;	/* KVA of page table */
	struct ste		*pm_stab;	/* KVA of segment table */
	int			pm_stchanged;	/* ST changed */
	short			pm_sref;	/* segment table ref count */
	short			pm_count;	/* pmap reference count */
	simple_lock_data_t	pm_lock;	/* lock on pmap */
	struct pmap_statistics	pm_stats;	/* pmap statistics */
	long			pm_ptpages;	/* more stats: PT pages */
};

typedef struct pmap	*pmap_t;

extern pmap_t		kernel_pmap;

/*
 * Macros for speed
 */
#define PMAP_ACTIVATE(pmapp, pcbp, iscurproc) \
	if ((pmapp) != NULL && (pmapp)->pm_stchanged) { \
		(pcbp)->pcb_ustp = \
		    hp300_btop(pmap_extract(kernel_pmap, (pmapp)->pm_stab)); \
		if (iscurproc) \
			loadustp((pcbp)->pcb_ustp); \
		(pmapp)->pm_stchanged = FALSE; \
	}
#define PMAP_DEACTIVATE(pmapp, pcbp)

/*
 * For each vm_page_t, there is a list of all currently valid virtual
 * mappings of that page.  An entry is a pv_entry_t, the list is pv_table.
 */
typedef struct pv_entry {
	struct pv_entry	*pv_next;	/* next pv_entry */
	struct pmap	*pv_pmap;	/* pmap where mapping lies */
	vm_offset_t	pv_va;		/* virtual address for mapping */
	struct ste	*pv_ptste;	/* non-zero if VA maps a PT page */
	struct pmap	*pv_ptpmap;	/* if pv_ptste, pmap for PT page */
	int		pv_flags;	/* flags */
} *pv_entry_t;

#define	PV_CI		0x01	/* all entries must be cache inhibited */
#define PV_PTPAGE	0x02	/* entry maps a page table page */

#ifdef	KERNEL
pv_entry_t	pv_table;		/* array of entries, one per page */

#define pa_index(pa)		atop(pa - vm_first_phys)
#define pa_to_pvh(pa)		(&pv_table[pa_index(pa)])

#define	pmap_resident_count(pmap)	((pmap)->pm_stats.resident_count)

extern	struct pte *Sysmap;
extern	char *vmmap;			/* map for mem, dumps, etc. */
#endif	KERNEL

#endif	_PMAP_MACHINE_
