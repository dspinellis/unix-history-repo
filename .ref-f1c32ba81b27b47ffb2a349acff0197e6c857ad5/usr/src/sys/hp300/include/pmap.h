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
 *	@(#)pmap.h	7.2 (Berkeley) %G%
 */

#ifndef	_PMAP_MACHINE_
#define	_PMAP_MACHINE_	1

#include <sys/lock.h>
#include <hp300/include/vmparam.h>
#include <vm/vm_statistics.h>

/*
 * HP300 hardware segment/page table entries
 */

struct ste {
	unsigned int	sg_pfnum:20;	/* page table frame number */
	unsigned int	:8;		/* reserved at 0 */
	unsigned int	:1;		/* reserved at 1 */
	unsigned int	sg_prot:1;	/* write protect bit */
	unsigned int	sg_v:2;		/* valid bits */
};

struct pte {
	unsigned int	pg_pfnum:20;	/* page frame number or 0 */
	unsigned int	:3;
	unsigned int	pg_w:1;		/* is wired */
	unsigned int	:1;		/* reserved at zero */
	unsigned int	pg_ci:1;	/* cache inhibit bit */
	unsigned int	:1;		/* reserved at zero */
	unsigned int	pg_m:1;		/* hardware modified (dirty) bit */
	unsigned int	pg_u:1;		/* hardware used (reference) bit */
	unsigned int	pg_prot:1;	/* write protect bit */
	unsigned int	pg_v:2;		/* valid bit */
};

typedef struct ste	st_entry_t;	/* segment table entry */
typedef struct pte	pt_entry_t;	/* Mach page table entry */

#define	PT_ENTRY_NULL	((pt_entry_t *) 0)
#define	ST_ENTRY_NULL	((st_entry_t *) 0)

#define	SG_V		0x00000002	/* segment is valid */
#define	SG_NV		0x00000000
#define	SG_PROT		0x00000004	/* access protection mask */
#define	SG_RO		0x00000004
#define	SG_RW		0x00000000
#define	SG_FRAME	0xfffff000
#define	SG_IMASK	0xffc00000
#define	SG_PMASK	0x003ff000
#define	SG_ISHIFT	22
#define	SG_PSHIFT	12

#define	PG_V		0x00000001
#define	PG_NV		0x00000000
#define	PG_PROT		0x00000004
#define	PG_U		0x00000008
#define	PG_M		0x00000010
#define	PG_W		0x00000100
#define	PG_RO		0x00000004
#define	PG_RW		0x00000000
#define	PG_FRAME	0xfffff000
#define	PG_CI		0x00000040
#define PG_SHIFT	12
#define	PG_PFNUM(x)	(((x) & PG_FRAME) >> PG_SHIFT)

#define HP_PAGE_SIZE	NBPG
#define HP_SEG_SIZE	NBSEG

#define HP_STSIZE	HP_PAGE_SIZE	/* segment table size */
#define HP_MAX_PTSIZE	HP_SEG_SIZE	/* max size of UPT */
#define HP_MAX_KPTSIZE	0x100000	/* max memory to allocate to KPT */
#define HP_PTBASE	0x10000000	/* UPT map base address */
#define HP_PTMAXSIZE	0x70000000	/* UPT map maximum size */

/*
 * Kernel virtual address to page table entry and to physical address.
 */
#define	kvtopte(va) \
	(&Sysmap[((unsigned)(va) - VM_MIN_KERNEL_ADDRESS) >> PGSHIFT])
#define	ptetokv(pt) \
	((((pt_entry_t *)(pt) - Sysmap) << PGSHIFT) + VM_MIN_KERNEL_ADDRESS)
#define	kvtophys(va) \
	((kvtopte(va)->pg_pfnum << PGSHIFT) | ((int)(va) & PGOFSET))

/*
 * Pmap stuff
 */
#define PMAP_NULL	((pmap_t) 0)

struct pmap {
	pt_entry_t		*pm_ptab;	/* KVA of page table */
	st_entry_t		*pm_stab;	/* KVA of segment table */
	boolean_t		pm_stchanged;	/* ST changed */
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
#define PMAP_ACTIVATE(pmapp, pcbp) \
	if ((pmapp) != PMAP_NULL && (pmapp)->pm_stchanged) { \
		(pcbp)->pcb_ustp = \
		    hp300_btop(pmap_extract(kernel_pmap, (pmapp)->pm_stab)); \
		if ((pmapp) == u.u_procp->p_map->pmap) \
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
	pmap_t		pv_pmap;	/* pmap where mapping lies */
	vm_offset_t	pv_va;		/* virtual address for mapping */
	st_entry_t	*pv_ptste;	/* non-zero if VA maps a PT page */
	pmap_t		pv_ptpmap;	/* if pv_ptste, pmap for PT page */
	int		pv_flags;	/* flags */
} *pv_entry_t;

#define	PV_ENTRY_NULL	((pv_entry_t) 0)

#define	PV_CI		0x01	/* all entries must be cache inhibited */
#define PV_PTPAGE	0x02	/* entry maps a page table page */

#ifdef	KERNEL

pv_entry_t	pv_table;		/* array of entries, one per page */

#define pa_index(pa)		atop(pa - vm_first_phys)
#define pa_to_pvh(pa)		(&pv_table[pa_index(pa)])

#define	pmap_resident_count(pmap)	((pmap)->pm_stats.resident_count)

extern	pt_entry_t	*Sysmap;
#endif	KERNEL

#endif	_PMAP_MACHINE_
