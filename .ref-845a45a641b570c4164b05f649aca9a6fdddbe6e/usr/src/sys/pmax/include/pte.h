/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: pte.h 1.11 89/09/03$
 *
 *	@(#)pte.h	7.3 (Berkeley) %G%
 */

/*
 * R2000 hardware page table entry
 */

#ifndef LOCORE
struct pte {
#if BYTE_ORDER == BIG_ENDIAN
unsigned int	pg_pfnum:20,		/* HW: core page frame number or 0 */
		pg_n:1,			/* HW: non-cacheable bit */
		pg_m:1,			/* HW: modified (dirty) bit */
		pg_v:1,			/* HW: valid bit */
		pg_g:1,			/* HW: ignore pid bit */
		:4,
		pg_swapm:1,		/* SW: page must be forced to swap */
		pg_fod:1,		/* SW: is fill on demand (=0) */
		pg_prot:2;		/* SW: access control */
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
unsigned int	pg_prot:2,		/* SW: access control */
		pg_fod:1,		/* SW: is fill on demand (=0) */
		pg_swapm:1,		/* SW: page must be forced to swap */
		:4,
		pg_g:1,			/* HW: ignore pid bit */
		pg_v:1,			/* HW: valid bit */
		pg_m:1,			/* HW: modified (dirty) bit */
		pg_n:1,			/* HW: non-cacheable bit */
		pg_pfnum:20;		/* HW: core page frame number or 0 */
#endif
};

typedef union pt_entry {
	unsigned int	pt_entry;	/* for copying, etc. */
	struct pte	pt_pte;		/* for getting to bits by name */
} pt_entry_t;	/* Mach page table entry */
#endif /* LOCORE */

#define	PT_ENTRY_NULL	((pt_entry_t *) 0)

#define	PG_PROT		0x00000003
#define PG_RW		0x00000000
#define PG_RO		0x00000001
#define PG_WIRED	0x00000002
#define	PG_G		0x00000100
#define	PG_V		0x00000200
#define	PG_NV		0x00000000
#define	PG_M		0x00000400
#define	PG_N		0x00000800
#define	PG_FRAME	0xfffff000
#define PG_SHIFT	12
#define	PG_PFNUM(x)	(((x) & PG_FRAME) >> PG_SHIFT)

#if defined(KERNEL) && !defined(LOCORE)
/*
 * Kernel virtual address to page table entry and visa versa.
 */
#define	kvtopte(va) \
	(Sysmap + (((vm_offset_t)(va) - VM_MIN_KERNEL_ADDRESS) >> PGSHIFT))
#define	ptetokv(pte) \
	((((pt_entry_t *)(pte) - Sysmap) << PGSHIFT) + VM_MIN_KERNEL_ADDRESS)

extern	pt_entry_t *Sysmap;		/* kernel pte table */
extern	u_int Sysmapsize;		/* number of pte's in Sysmap */
#endif
