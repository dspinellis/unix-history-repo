/* 
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	OMRON: $Id: pmap.c,v 1.2 92/06/14 06:19:40 moti Exp $
 *
 * from: hp300/hp300/pmap.c	7.11 (Berkeley) 7/12/92
 *
 *	@(#)pmap.c	7.2 (Berkeley) %G%
 */

/*
 *	LUNA physical map management code taken from:
 *	HP9000/300 series physical map management code.
 *
 *	XXX will only work for PAGE_SIZE == NBPG (i.e. 4096 bytes).
 *	Hence, there is no point in defining DYNPGSIZE.
 */

/*
 *	Manages physical address maps.
 *
 *	In addition to hardware address maps, this
 *	module is called upon to provide software-use-only
 *	maps which may or may not be stored in the same
 *	form as hardware maps.  These pseudo-maps are
 *	used to store intermediate results from copy
 *	operations to and from address spaces.
 *
 *	Since the information managed by this module is
 *	also stored by the logical address mapping module,
 *	this module may throw away valid virtual-to-physical
 *	mappings at almost any time.  However, invalidations
 *	of virtual-to-physical mappings must be done as
 *	requested.
 *
 *	In order to cope with hardware architectures which
 *	make virtual-to-physical map invalidates expensive,
 *	this module may delay invalidate or reduced protection
 *	operations until such time as they are actually
 *	necessary.  This module is given full information as
 *	to which processors are currently using which maps,
 *	and to when physical maps must be made correct.
 */

#include "param.h"
#include "systm.h"
#include "proc.h"
#include "malloc.h"
#include "user.h"

#include "pte.h"

#include "vm/vm.h"
#include "vm/vm_kern.h"
#include "vm/vm_page.h"

#include "../include/cpu.h"

#ifdef DEBUG
struct {
	int collectscans;
	int collectpages;
	int kpttotal;
	int kptinuse;
	int kptmaxuse;
} kpt_stats;
struct {
	int kernel;	/* entering kernel mapping */
	int user;	/* entering user mapping */
	int ptpneeded;	/* needed to allocate a PT page */
	int pwchange;	/* no mapping change, just wiring or protection */
	int wchange;	/* no mapping change, just wiring */
	int mchange;	/* was mapped but mapping to different page */
	int managed;	/* a managed page */
	int firstpv;	/* first mapping for this PA */
	int secondpv;	/* second mapping for this PA */
	int ci;		/* cache inhibited */
	int unmanaged;	/* not a managed page */
	int flushes;	/* cache flushes */
} enter_stats;
struct {
	int calls;
	int removes;
	int pvfirst;
	int pvsearch;
	int ptinvalid;
	int uflushes;
	int sflushes;
} remove_stats;
struct {
	int calls;
	int pages;
	int alreadyro;
	int alreadyrw;
} protect_stats;

int debugmap = 0;
int pmapdebug = 0x2000;
#define PDB_FOLLOW	0x0001
#define PDB_INIT	0x0002
#define PDB_ENTER	0x0004
#define PDB_REMOVE	0x0008
#define PDB_CREATE	0x0010
#define PDB_PTPAGE	0x0020
#define PDB_CACHE	0x0040
#define PDB_BITS	0x0080
#define PDB_COLLECT	0x0100
#define PDB_PROTECT	0x0200
#define PDB_SEGTAB	0x0400
#define PDB_PARANOIA	0x2000
#define PDB_WIRING	0x4000
#define PDB_PVDUMP	0x8000

extern vm_offset_t pager_sva, pager_eva;
#endif

/*
 * Get STEs and PTEs for user/kernel address space
 */
#define	pmap_ste(m, v)	(&((m)->pm_stab[(vm_offset_t)(v) >> SG_ISHIFT]))
#define pmap_pte(m, v)	(&((m)->pm_ptab[(vm_offset_t)(v) >> PG_SHIFT]))

#define pmap_ste_v(m, v)	(pmap_ste(m, v)->sg_v)

#define pmap_pte_pa(pte)	(*(int *)(pte) & PG_FRAME)
#define pmap_pte_w(pte)		((pte)->pg_w)
#define pmap_pte_ci(pte)	((pte)->pg_ci)
#define pmap_pte_m(pte)		((pte)->pg_m)
#define pmap_pte_u(pte)		((pte)->pg_u)
#define pmap_pte_prot(pte)	((pte)->pg_prot)
#define pmap_pte_v(pte)		((pte)->pg_v)
#define pmap_pte_set_w(pte, v)		((pte)->pg_w = (v))
#define pmap_pte_set_prot(pte, v)	((pte)->pg_prot = (v))

/*
 * Given a map and a machine independent protection code,
 * convert to a vax protection code.
 */
#define pte_prot(m, p)	(protection_codes[p])
int	protection_codes[8];

/*
 * Kernel page table page management.
 */
struct kpt_page {
	struct kpt_page *kpt_next;	/* link on either used or free list */
	vm_offset_t	kpt_va;		/* always valid kernel VA */
	vm_offset_t	kpt_pa;		/* PA of this page (for speed) */
};
struct kpt_page *kpt_free_list, *kpt_used_list;
struct kpt_page *kpt_pages;

/*
 * Kernel segment/page table and page table map.
 * The page table map gives us a level of indirection we need to dynamically
 * expand the page table.  It is essentially a copy of the segment table
 * with PTEs instead of STEs.  All are initialized in locore at boot time.
 * Sysmap will initially contain VM_KERNEL_PT_PAGES pages of PTEs.
 * Segtabzero is an empty segment table which all processes share til they
 * reference something.
 */
st_entry_t	*Sysseg;
pt_entry_t	*Sysmap, *Sysptmap;
st_entry_t	*Segtabzero, *Segtabzeropa;
vm_size_t	Sysptsize = VM_KERNEL_PT_PAGES;

struct pmap	kernel_pmap_store;
vm_map_t	pt_map;

vm_offset_t    	avail_start;	/* PA of first available physical page */
vm_offset_t	avail_end;	/* PA of last available physical page */
vm_size_t	mem_size;	/* memory size in bytes */
vm_offset_t	virtual_avail;  /* VA of first avail page (after kernel bss)*/
vm_offset_t	virtual_end;	/* VA of last avail page (end of kernel AS) */
vm_offset_t	vm_first_phys;	/* PA of first managed page */
vm_offset_t	vm_last_phys;	/* PA just past last managed page */
#if defined(DYNPGSIZE)
int		lunapagesperpage;	/* PAGE_SIZE / LUNA_PAGE_SIZE */
#endif
boolean_t	pmap_initialized = FALSE;	/* Has pmap_init completed? */
char		*pmap_attributes;	/* reference and modify bits */

boolean_t	pmap_testbit();
void		pmap_enter_ptpage();

extern	struct physmap io_physmap[];   	/* LUNA: io mapping information */

/*
 * Bootstrap memory allocator. This function allows for early dynamic
 * memory allocation until the virtual memory system has been bootstrapped.
 * After that point, either kmem_alloc or malloc should be used. This
 * function works by stealing pages from the (to be) managed page pool,
 * stealing virtual address space, then mapping the pages and zeroing them.
 *
 * It should be used from pmap_bootstrap till vm_page_startup, afterwards
 * it cannot be used, and will generate a panic if tried. Note that this
 * memory will never be freed, and in essence it is wired down.
 */
void *
pmap_bootstrap_alloc(size) {
	vm_offset_t val;
	int i;
	extern boolean_t vm_page_startup_initialized;
	
	if (vm_page_startup_initialized)
		panic("pmap_bootstrap_alloc: called after startup initialized");
	size = round_page(size);
	val = virtual_avail;

	virtual_avail = pmap_map(virtual_avail, avail_start,
		avail_start + size, VM_PROT_READ|VM_PROT_WRITE);
	avail_start += size;

	blkclr ((caddr_t) val, size);
	return ((void *) val);
}

/*
 *	Initialize the pmap module.
 *	Called by vm_init, to initialize any structures that the pmap
 *	system needs to map virtual memory.
 */
void
pmap_init(phys_start, phys_end)
	vm_offset_t	phys_start, phys_end;
{
	vm_offset_t	addr, addr2;
	vm_size_t	npg, s;
	int		rv, index;
	extern char kstack[];

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_init(%x, %x)\n", phys_start, phys_end);
#endif
	/*
	 * Now that kernel map has been allocated, we can mark as
	 * unavailable regions which we have mapped in locore.
	 */
	for(index = 0; io_physmap[index].pm_phys; index++)
	  {
	      addr = io_physmap[index].pm_phys;
	      (void) vm_map_find(kernel_map, NULL, (vm_offset_t) 0,
				 &addr, luna_round_page(io_physmap[index].pm_size), FALSE);
	      if (addr != io_physmap[index].pm_phys)
		goto bogons;
	  }

	addr = (vm_offset_t) Sysmap;
	vm_object_reference(kernel_object);
	(void) vm_map_find(kernel_map, kernel_object, addr,
			   &addr, LUNA_MAX_PTSIZE, FALSE);
	/*
	 * If this fails it is probably because the static portion of
	 * the kernel page table isn't big enough and we overran the
	 * page table map.   Need to adjust pmap_size() in luna_init.c.
	 */
	if (addr != (vm_offset_t)Sysmap)
		goto bogons;

	addr = (vm_offset_t) kstack;
	vm_object_reference(kernel_object);
	(void) vm_map_find(kernel_map, kernel_object, addr,
			   &addr, luna_ptob(UPAGES), FALSE);
	if (addr != (vm_offset_t)kstack)
bogons:
		panic("pmap_init: bogons in the VM system!\n");

#ifdef DEBUG
	if (pmapdebug & PDB_INIT) {
		printf("pmap_init: Sysseg %x, Sysmap %x, Sysptmap %x\n",
		       Sysseg, Sysmap, Sysptmap);
		printf("  pstart %x, pend %x, vstart %x, vend %x\n",
		       avail_start, avail_end, virtual_avail, virtual_end);
	}
#endif

	/*
	 * Allocate memory for random pmap data structures.  Includes the
	 * initial segment table, pv_head_table and pmap_attributes.
	 */
	npg = atop(phys_end - phys_start);
	s = (vm_size_t) (LUNA_STSIZE + sizeof(struct pv_entry) * npg + npg);
	s = round_page(s);
	addr = (vm_offset_t) kmem_alloc(kernel_map, s);
	Segtabzero = (st_entry_t *) addr;
	Segtabzeropa = (st_entry_t *) pmap_extract(kernel_pmap, addr);
	addr += LUNA_STSIZE;
	pv_table = (pv_entry_t) addr;
	addr += sizeof(struct pv_entry) * npg;
	pmap_attributes = (char *) addr;
#ifdef DEBUG
	if (pmapdebug & PDB_INIT)
		printf("pmap_init: %x bytes: npg %x s0 %x(%x) tbl %x atr %x\n",
		       s, npg, Segtabzero, Segtabzeropa,
		       pv_table, pmap_attributes);
#endif

	/*
	 * Allocate physical memory for kernel PT pages and their management.
	 * We need 1 PT page per possible task plus some slop.
	 */
	npg = min(atop(LUNA_MAX_KPTSIZE), maxproc+16);
	s = ptoa(npg) + round_page(npg * sizeof(struct kpt_page));

	/*
	 * Verify that space will be allocated in region for which
	 * we already have kernel PT pages.
	 */
	addr = 0;
	rv = vm_map_find(kernel_map, NULL, 0, &addr, s, TRUE);
	if (rv != KERN_SUCCESS || addr + s >= (vm_offset_t)Sysmap)
		panic("pmap_init: kernel PT too small");
	vm_map_remove(kernel_map, addr, addr + s);

	/*
	 * Now allocate the space and link the pages together to
	 * form the KPT free list.
	 */
	addr = (vm_offset_t) kmem_alloc(kernel_map, s);
	s = ptoa(npg);
	addr2 = addr + s;
	kpt_pages = &((struct kpt_page *)addr2)[npg];
	kpt_free_list = (struct kpt_page *) 0;
	do {
		addr2 -= LUNA_PAGE_SIZE;
		(--kpt_pages)->kpt_next = kpt_free_list;
		kpt_free_list = kpt_pages;
		kpt_pages->kpt_va = addr2;
		kpt_pages->kpt_pa = pmap_extract(kernel_pmap, addr2);
	} while (addr != addr2);
#ifdef DEBUG
	kpt_stats.kpttotal = atop(s);
	if (pmapdebug & PDB_INIT)
		printf("pmap_init: KPT: %d pages from %x to %x\n",
		       atop(s), addr, addr + s);
#endif

	/*
	 * Slightly modified version of kmem_suballoc() to get page table
	 * map where we want it.
	 */
	addr = LUNA_PTBASE;
	s = min(LUNA_PTMAXSIZE, maxproc*LUNA_MAX_PTSIZE);
	addr2 = addr + s;
	rv = vm_map_find(kernel_map, NULL, 0, &addr, s, TRUE);
	if (rv != KERN_SUCCESS)
		panic("pmap_init: cannot allocate space for PT map");
	pmap_reference(vm_map_pmap(kernel_map));
	pt_map = vm_map_create(vm_map_pmap(kernel_map), addr, addr2, TRUE);
	if (pt_map == NULL)
		panic("pmap_init: cannot create pt_map");
	rv = vm_map_submap(kernel_map, addr, addr2, pt_map);
	if (rv != KERN_SUCCESS)
		panic("pmap_init: cannot map range to pt_map");
#ifdef DEBUG
	if (pmapdebug & PDB_INIT)
		printf("pmap_init: pt_map [%x - %x)\n", addr, addr2);
#endif

	/*
	 * Now it is safe to enable pv_table recording.
	 */
	vm_first_phys = phys_start;
	vm_last_phys = phys_end;
	pmap_initialized = TRUE;
}

/*
 *	Used to map a range of physical addresses into kernel
 *	virtual address space.
 *
 *	For now, VM is already on, we only need to map the
 *	specified memory.
 */
vm_offset_t
pmap_map(virt, start, end, prot)
	vm_offset_t	virt;
	vm_offset_t	start;
	vm_offset_t	end;
	int		prot;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_map(%x, %x, %x, %x)\n", virt, start, end, prot);
#endif
	while (start < end) {
		pmap_enter(kernel_pmap, virt, start, prot, FALSE);
		virt += PAGE_SIZE;
		start += PAGE_SIZE;
	}
	return(virt);
}

/*
 *	Create and return a physical map.
 *
 *	If the size specified for the map
 *	is zero, the map is an actual physical
 *	map, and may be referenced by the
 *	hardware.
 *
 *	If the size specified is non-zero,
 *	the map will be used in software only, and
 *	is bounded by that size.
 */
pmap_t
pmap_create(size)
	vm_size_t	size;
{
	register pmap_t pmap;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_CREATE))
		printf("pmap_create(%x)\n", size);
#endif
	/*
	 * Software use map does not need a pmap
	 */
	if (size)
		return(NULL);

	/* XXX: is it ok to wait here? */
	pmap = (pmap_t) malloc(sizeof *pmap, M_VMPMAP, M_WAITOK);
#ifdef notifwewait
	if (pmap == NULL)
		panic("pmap_create: cannot allocate a pmap");
#endif
	bzero(pmap, sizeof(*pmap));
	pmap_pinit(pmap);
	return (pmap);
}

/*
 * Initialize a preallocated and zeroed pmap structure,
 * such as one in a vmspace structure.
 */
void
pmap_pinit(pmap)
	register struct pmap *pmap;
{

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_CREATE))
		printf("pmap_pinit(%x)\n", pmap);
#endif
	/*
	 * No need to allocate page table space yet but we do need a
	 * valid segment table.  Initially, we point everyone at the
	 * "null" segment table.  On the first pmap_enter, a real
	 * segment table will be allocated.
	 */
	pmap->pm_stab = Segtabzero;
	pmap->pm_stpa = Segtabzeropa;
	pmap->pm_stchanged = TRUE;
	pmap->pm_count = 1;
	simple_lock_init(&pmap->pm_lock);
}

/*
 *	Retire the given physical map from service.
 *	Should only be called if the map contains
 *	no valid mappings.
 */
void
pmap_destroy(pmap)
	register pmap_t pmap;
{
	int count;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_destroy(%x)\n", pmap);
#endif
	if (pmap == NULL)
		return;

	simple_lock(&pmap->pm_lock);
	count = --pmap->pm_count;
	simple_unlock(&pmap->pm_lock);
	if (count == 0) {
		pmap_release(pmap);
		free((caddr_t)pmap, M_VMPMAP);
	}
}

/*
 * Release any resources held by the given physical map.
 * Called when a pmap initialized by pmap_pinit is being released.
 * Should only be called if the map contains no valid mappings.
 */
void
pmap_release(pmap)
	register struct pmap *pmap;
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_release(%x)\n", pmap);
#endif
#ifdef notdef /* DIAGNOSTIC */
	/* count would be 0 from pmap_destroy... */
	simple_lock(&pmap->pm_lock);
	if (pmap->pm_count != 1)
		panic("pmap_release count");
#endif
	if (pmap->pm_ptab)
		kmem_free_wakeup(pt_map, (vm_offset_t)pmap->pm_ptab,
				 LUNA_MAX_PTSIZE);
	if (pmap->pm_stab != Segtabzero)
		kmem_free(kernel_map, (vm_offset_t)pmap->pm_stab, LUNA_STSIZE);
}

/*
 *	Add a reference to the specified pmap.
 */
void
pmap_reference(pmap)
	pmap_t	pmap;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_reference(%x)\n", pmap);
#endif
	if (pmap != NULL) {
		simple_lock(&pmap->pm_lock);
		pmap->pm_count++;
		simple_unlock(&pmap->pm_lock);
	}
}

/*
 *	Remove the given range of addresses from the specified map.
 *
 *	It is assumed that the start and end are properly
 *	rounded to the page size.
 */
void
pmap_remove(pmap, sva, eva)
	register pmap_t pmap;
	vm_offset_t sva, eva;
{
	register vm_offset_t pa, va;
	register pt_entry_t *pte;
	register pv_entry_t pv, npv;
	pmap_t ptpmap;
	int *ste, s, bits;
	boolean_t firstpage = TRUE;
	boolean_t flushcache = FALSE;
#ifdef DEBUG
	pt_entry_t opte;

	if (pmapdebug & (PDB_FOLLOW|PDB_REMOVE|PDB_PROTECT))
		printf("pmap_remove(%x, %x, %x)\n", pmap, sva, eva);
#endif

	if (pmap == NULL)
		return;

#ifdef DEBUG
	remove_stats.calls++;
#endif
	for (va = sva; va < eva; va += PAGE_SIZE) {
		/*
		 * Weed out invalid mappings.
		 * Note: we assume that the segment table is always allocated.
		 */
		if (!pmap_ste_v(pmap, va)) {
			/* XXX: avoid address wrap around */
			if (va >= luna_trunc_seg(VM_MAX_ADDRESS))
				break;
			va = luna_round_seg(va + PAGE_SIZE) - PAGE_SIZE;
			continue;
		}
		pte = pmap_pte(pmap, va);
		pa = pmap_pte_pa(pte);
		if (pa == 0)
			continue;
#ifdef DEBUG
		opte = *pte;
		remove_stats.removes++;
#endif
		/*
		 * Update statistics
		 */
		if (pmap_pte_w(pte))
			pmap->pm_stats.wired_count--;
		pmap->pm_stats.resident_count--;

		/*
		 * Invalidate the PTEs.
		 * XXX: should cluster them up and invalidate as many
		 * as possible at once.
		 */
#ifdef DEBUG
		if (pmapdebug & PDB_REMOVE)
			printf("remove: invalidating pte at %x\n", pte);
			remove_stats.sflushes++;
#endif
#if defined(DYNPGSIZE)
		{
			register int ix = 0;

			bits = 0;
			do {
				bits |= *(int *)pte & (PG_U|PG_M);
				*(int *)pte++ = PG_NV;
				TBIS(va + ix * LUNA_PAGE_SIZE);
			} while (++ix != lunapagesperpage);
		}
#else
		bits = *(int *)pte & (PG_U|PG_M);
		*(int *)pte = PG_NV;
		TBIS(va);
#endif
		/*
		 * For user mappings decrement the wiring count on
		 * the PT page.  We do this after the PTE has been
		 * invalidated because vm_map_pageable winds up in
		 * pmap_pageable which clears the modify bit for the
		 * PT page.
		 */
		if (pmap != kernel_pmap) {
			pte = pmap_pte(pmap, va);
			vm_map_pageable(pt_map, trunc_page(pte),
					round_page(pte+1), TRUE);
#ifdef DEBUG
			if (pmapdebug & PDB_WIRING)
				pmap_check_wiring("remove", trunc_page(pte));
#endif
		}
		/*
		 * Remove from the PV table (raise IPL since we
		 * may be called at interrupt time).
		 */
		if (pa < vm_first_phys || pa >= vm_last_phys)
			continue;
		pv = pa_to_pvh(pa);
		ste = (int *)0;
		s = splimp();
		/*
		 * If it is the first entry on the list, it is actually
		 * in the header and we must copy the following entry up
		 * to the header.  Otherwise we must search the list for
		 * the entry.  In either case we free the now unused entry.
		 */
		if (pmap == pv->pv_pmap && va == pv->pv_va) {
			ste = (int *)pv->pv_ptste;
			ptpmap = pv->pv_ptpmap;
			npv = pv->pv_next;
			if (npv) {
				*pv = *npv;
				free((caddr_t)npv, M_VMPVENT);
			} else
				pv->pv_pmap = NULL;
#ifdef DEBUG
			remove_stats.pvfirst++;
#endif
		} else {
			for (npv = pv->pv_next; npv; npv = npv->pv_next) {
#ifdef DEBUG
				remove_stats.pvsearch++;
#endif
				if (pmap == npv->pv_pmap && va == npv->pv_va)
					break;
				pv = npv;
			}
#ifdef DEBUG
			if (npv == NULL)
				panic("pmap_remove: PA not in pv_tab");
#endif
			ste = (int *)npv->pv_ptste;
			ptpmap = npv->pv_ptpmap;
			pv->pv_next = npv->pv_next;
			free((caddr_t)npv, M_VMPVENT);
			pv = pa_to_pvh(pa);
		}
		/*
		 * If only one mapping left we no longer need to cache inhibit
		 */
		if (pv->pv_pmap &&
		    pv->pv_next == NULL && (pv->pv_flags & PV_CI)) {
#ifdef DEBUG
			if (pmapdebug & PDB_CACHE)
				printf("remove: clearing CI for pa %x\n", pa);
#endif
			pv->pv_flags &= ~PV_CI;
			pmap_changebit(pa, PG_CI, FALSE);
#ifdef DEBUG
			if ((pmapdebug & (PDB_CACHE|PDB_PVDUMP)) ==
			    (PDB_CACHE|PDB_PVDUMP))
				pmap_pvdump(pa);
#endif
		}

		/*
		 * If this was a PT page we must also remove the
		 * mapping from the associated segment table.
		 */
		if (ste) {
#ifdef DEBUG
			remove_stats.ptinvalid++;
			if (pmapdebug & (PDB_REMOVE|PDB_PTPAGE)) {
				printf("remove: ste was %x@%x pte was %x@%x\n",
				       *ste, ste,
				       *(int *)&opte, pmap_pte(pmap, va));
			}
#endif
			*ste = SG_NV;
			/*
			 * If it was a user PT page, we decrement the
			 * reference count on the segment table as well,
			 * freeing it if it is now empty.
			 */
			if (ptpmap != kernel_pmap) {
#ifdef DEBUG
				if (pmapdebug & (PDB_REMOVE|PDB_SEGTAB))
					printf("remove: stab %x, refcnt %d\n",
					       ptpmap->pm_stab,
					       ptpmap->pm_sref - 1);
				if ((pmapdebug & PDB_PARANOIA) &&
				    ptpmap->pm_stab != (st_entry_t *)trunc_page(ste))
					panic("remove: bogus ste");
#endif
				if (--(ptpmap->pm_sref) == 0) {
#ifdef DEBUG
					if (pmapdebug&(PDB_REMOVE|PDB_SEGTAB))
					printf("remove: free stab %x\n",
					       ptpmap->pm_stab);
#endif
					kmem_free(kernel_map,
						  (vm_offset_t)ptpmap->pm_stab,
						  LUNA_STSIZE);
					ptpmap->pm_stab = Segtabzero;
					ptpmap->pm_stpa = Segtabzeropa;
					ptpmap->pm_stchanged = TRUE;
					/*
					 * XXX may have changed segment table
					 * pointer for current process so
					 * update now to reload hardware.
					 */
					if (ptpmap == curproc->p_vmspace->vm_map.pmap)
						PMAP_ACTIVATE(ptpmap,
							(struct pcb *)curproc->p_addr, 1);
				}
			}
			if (ptpmap == kernel_pmap)
				TBIAS();
			else
				TBIAU();
			pv->pv_flags &= ~PV_PTPAGE;
			ptpmap->pm_ptpages--;
		}
		/*
		 * Update saved attributes for managed page
		 */
		pmap_attributes[pa_index(pa)] |= bits;
		splx(s);
	}
}

/*
 *	pmap_page_protect:
 *
 *	Lower the permission for all mappings to a given page.
 */
void
pmap_page_protect(pa, prot)
	vm_offset_t	pa;
	vm_prot_t	prot;
{
	register pv_entry_t pv;
	int s;

#ifdef DEBUG
	if ((pmapdebug & (PDB_FOLLOW|PDB_PROTECT)) ||
	    prot == VM_PROT_NONE && (pmapdebug & PDB_REMOVE))
		printf("pmap_page_protect(%x, %x)\n", pa, prot);
#endif
	if (pa < vm_first_phys || pa >= vm_last_phys)
		return;

	switch (prot) {
	case VM_PROT_ALL:
		break;
	/* copy_on_write */
	case VM_PROT_READ:
	case VM_PROT_READ|VM_PROT_EXECUTE:
		pmap_changebit(pa, PG_RO, TRUE);
		break;
	/* remove_all */
	default:
		pv = pa_to_pvh(pa);
		s = splimp();
		while (pv->pv_pmap != NULL) {
#ifdef DEBUG
			if (!pmap_ste_v(pv->pv_pmap, pv->pv_va) ||
			    pmap_pte_pa(pmap_pte(pv->pv_pmap,pv->pv_va)) != pa)
				panic("pmap_page_protect: bad mapping");
#endif
			pmap_remove(pv->pv_pmap, pv->pv_va,
				    pv->pv_va + PAGE_SIZE);
		}
		splx(s);
		break;
	}
}

/*
 *	Set the physical protection on the
 *	specified range of this map as requested.
 */
void
pmap_protect(pmap, sva, eva, prot)
	register pmap_t	pmap;
	vm_offset_t	sva, eva;
	vm_prot_t	prot;
{
	register pt_entry_t *pte;
	register vm_offset_t va;
	int lunaprot;
	boolean_t firstpage = TRUE;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_PROTECT))
		printf("pmap_protect(%x, %x, %x, %x)\n", pmap, sva, eva, prot);
	protect_stats.calls++;
#endif
	if (pmap == NULL)
		return;

	if ((prot & VM_PROT_READ) == VM_PROT_NONE) {
		pmap_remove(pmap, sva, eva);
		return;
	}
	if (prot & VM_PROT_WRITE)
		return;

	pte = pmap_pte(pmap, sva);
	lunaprot = pte_prot(pmap, prot) == PG_RO ? 1 : 0;
	for (va = sva; va < eva; va += PAGE_SIZE) {
		/*
		 * Page table page is not allocated.
		 * Skip it, we don't want to force allocation
		 * of unnecessary PTE pages just to set the protection.
		 */
		if (!pmap_ste_v(pmap, va)) {
			/* XXX: avoid address wrap around */
			if (va >= luna_trunc_seg((vm_offset_t)-1))
				break;
			va = luna_round_seg(va + PAGE_SIZE) - PAGE_SIZE;
#if defined(DYNPGSIZE)
			pte = pmap_pte(pmap, va) + lunapagesperpage;
#else
			pte = pmap_pte(pmap, va) + 1;
#endif
			continue;
		}
		/*
		 * Page not valid.  Again, skip it.
		 * Should we do this?  Or set protection anyway?
		 */
		if (!pmap_pte_v(pte)) {
#if defined(DYNPGSIZE)
			pte += lunapagesperpage;
#else
			pte++;
#endif
			continue;
		}
#if defined(DYNPGSIZE)
		{
			register int ix = 0;

			do {
				/*
				 * Clear caches as necessary if making RO.
				 * XXX clear VAC?  Doesn't seem to be needed.
				 */
#ifdef DEBUG
				protect_stats.pages++;
				if (lunaprot && pmap_pte_prot(pte))
					protect_stats.alreadyro++;
				if (!lunaprot && !pmap_pte_prot(pte))
					protect_stats.alreadyrw++;
#endif
				pmap_pte_set_prot(pte++, lunaprot);
				TBIS(va + ix * LUNA_PAGE_SIZE);
			} while (++ix != lunapagesperpage);
		}
#else
		/*
		 * Clear caches as necessary if making RO.
		 * XXX clear VAC?  Doesn't seem to be needed.
		 */
#ifdef DEBUG
		protect_stats.pages++;
		if (lunaprot && pmap_pte_prot(pte))
			protect_stats.alreadyro++;
		if (!lunaprot && !pmap_pte_prot(pte))
			protect_stats.alreadyrw++;
#endif
		pmap_pte_set_prot(pte++, lunaprot);
		TBIS(va);
#endif
	}
}

/*
 *	Insert the given physical page (p) at
 *	the specified virtual address (v) in the
 *	target physical map with the protection requested.
 *
 *	If specified, the page will be wired down, meaning
 *	that the related pte can not be reclaimed.
 *
 *	NB:  This is the only routine which MAY NOT lazy-evaluate
 *	or lose information.  That is, this routine must actually
 *	insert this page into the given map NOW.
 */
void
pmap_enter(pmap, va, pa, prot, wired)
	register pmap_t pmap;
	vm_offset_t va;
	register vm_offset_t pa;
	vm_prot_t prot;
	boolean_t wired;
{
	register pt_entry_t *pte;
	register int npte;
	vm_offset_t opa;
	boolean_t cacheable = TRUE;
	boolean_t checkpv = TRUE;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_ENTER))
		printf("pmap_enter(%x, %x, %x, %x, %x)\n",
		       pmap, va, pa, prot, wired);
#endif
	if (pmap == NULL)
		return;

#ifdef DEBUG
	if (pmap == kernel_pmap)
		enter_stats.kernel++;
	else
		enter_stats.user++;
#endif
	/*
	 * For user mapping, allocate kernel VM resources if necessary.
	 */
	if (pmap->pm_ptab == NULL)
		pmap->pm_ptab = (pt_entry_t *)
			kmem_alloc_wait(pt_map, LUNA_MAX_PTSIZE);

	/*
	 * Segment table entry not valid, we need a new PT page
	 */
	if (!pmap_ste_v(pmap, va))
		pmap_enter_ptpage(pmap, va);

	pte = pmap_pte(pmap, va);
	opa = pmap_pte_pa(pte);
#ifdef DEBUG
	if (pmapdebug & PDB_ENTER)
		printf("enter: pte %x, *pte %x\n", pte, *(int *)pte);
#endif

	/*
	 * Mapping has not changed, must be protection or wiring change.
	 */
	if (opa == pa) {
#ifdef DEBUG
		enter_stats.pwchange++;
#endif
		/*
		 * Wiring change, just update stats.
		 * We don't worry about wiring PT pages as they remain
		 * resident as long as there are valid mappings in them.
		 * Hence, if a user page is wired, the PT page will be also.
		 */
		if (wired && !pmap_pte_w(pte) || !wired && pmap_pte_w(pte)) {
#ifdef DEBUG
			if (pmapdebug & PDB_ENTER)
				printf("enter: wiring change -> %x\n", wired);
#endif
			if (wired)
				pmap->pm_stats.wired_count++;
			else
				pmap->pm_stats.wired_count--;
#ifdef DEBUG
			enter_stats.wchange++;
#endif
		}
		/*
		 * Retain cache inhibition status
		 */
		checkpv = FALSE;
		if (pmap_pte_ci(pte))
			cacheable = FALSE;
		goto validate;
	}

	/*
	 * Mapping has changed, invalidate old range and fall through to
	 * handle validating new mapping.
	 */
	if (opa) {
#ifdef DEBUG
		if (pmapdebug & PDB_ENTER)
			printf("enter: removing old mapping %x\n", va);
#endif
		pmap_remove(pmap, va, va + PAGE_SIZE);
#ifdef DEBUG
		enter_stats.mchange++;
#endif
	}

	/*
	 * If this is a new user mapping, increment the wiring count
	 * on this PT page.  PT pages are wired down as long as there
	 * is a valid mapping in the page.
	 */
	if (pmap != kernel_pmap)
		vm_map_pageable(pt_map, trunc_page(pte),
				round_page(pte+1), FALSE);

	/*
	 * Enter on the PV list if part of our managed memory
	 * Note that we raise IPL while manipulating pv_table
	 * since pmap_enter can be called at interrupt time.
	 */
	if (pa >= vm_first_phys && pa < vm_last_phys) {
		register pv_entry_t pv, npv;
		int s;

#ifdef DEBUG
		enter_stats.managed++;
#endif
		pv = pa_to_pvh(pa);
		s = splimp();
#ifdef DEBUG
		if (pmapdebug & PDB_ENTER)
			printf("enter: pv at %x: %x/%x/%x\n",
			       pv, pv->pv_va, pv->pv_pmap, pv->pv_next);
#endif
		/*
		 * No entries yet, use header as the first entry
		 */
		if (pv->pv_pmap == NULL) {
#ifdef DEBUG
			enter_stats.firstpv++;
#endif
			pv->pv_va = va;
			pv->pv_pmap = pmap;
			pv->pv_next = NULL;
			pv->pv_ptste = NULL;
			pv->pv_ptpmap = NULL;
			pv->pv_flags = 0;
		}
		/*
		 * There is at least one other VA mapping this page.
		 * Place this entry after the header.
		 */
		else {
#ifdef DEBUG
			for (npv = pv; npv; npv = npv->pv_next)
				if (pmap == npv->pv_pmap && va == npv->pv_va)
					panic("pmap_enter: already in pv_tab");
#endif
			npv = (pv_entry_t)
				malloc(sizeof *npv, M_VMPVENT, M_NOWAIT);
			npv->pv_va = va;
			npv->pv_pmap = pmap;
			npv->pv_next = pv->pv_next;
			npv->pv_ptste = NULL;
			npv->pv_ptpmap = NULL;
			pv->pv_next = npv;
#ifdef DEBUG
			if (!npv->pv_next)
				enter_stats.secondpv++;
#endif
		}
		splx(s);
	}
	/*
	 * Assumption: if it is not part of our managed memory
	 * then it must be device memory which may be volitile.
	 */
	else if (pmap_initialized) {
		checkpv = cacheable = FALSE;
#ifdef DEBUG
		enter_stats.unmanaged++;
#endif
	}

	/*
	 * Increment counters
	 */
	pmap->pm_stats.resident_count++;
	if (wired)
		pmap->pm_stats.wired_count++;

validate:
	/*
	 * Now validate mapping with desired protection/wiring.
	 * Assume uniform modified and referenced status for all
	 * LUNA pages in a MACH page.
	 */
	npte = (pa & PG_FRAME) | pte_prot(pmap, prot) | PG_V;
	npte |= (*(int *)pte & (PG_M|PG_U));
	if (wired)
		npte |= PG_W;
	if (!checkpv && !cacheable)
		npte |= PG_CI;
#ifdef DEBUG
	if (pmapdebug & PDB_ENTER)
		printf("enter: new pte value %x\n", npte);
#endif
#if defined(DYNPGSIZE)
	{
		register int ix = 0;

		do {
			*(int *)pte++ = npte;
			TBIS(va);
			npte += LUNA_PAGE_SIZE;
			va += LUNA_PAGE_SIZE;
		} while (++ix != lunapagesperpage);
	}
#else
	*(int *)pte = npte;
	TBIS(va);
#endif
	/*
	 * The following is executed if we are entering a second
	 * (or greater) mapping for a physical page and the mappings
	 * may create an aliasing problem.  In this case we must
	 * cache inhibit the descriptors involved and flush any
	 * external VAC.
	 */
	if (checkpv && !cacheable) {
		pmap_changebit(pa, PG_CI, TRUE);
#ifdef DEBUG
		enter_stats.flushes++;
#endif
#ifdef DEBUG
		if ((pmapdebug & (PDB_CACHE|PDB_PVDUMP)) ==
		    (PDB_CACHE|PDB_PVDUMP))
			pmap_pvdump(pa);
#endif
	}
#ifdef	DEEBUG
	if ((pmapdebug & PDB_WIRING) && pmap != kernel_pmap) {
#if defined(DYNPGSIZE)
		va -= PAGE_SIZE;
#endif
		pmap_check_wiring("enter", trunc_page(pmap_pte(pmap, va)));
	}
#endif
}

/*
 *	Routine:	pmap_change_wiring
 *	Function:	Change the wiring attribute for a map/virtual-address
 *			pair.
 *	In/out conditions:
 *			The mapping must already exist in the pmap.
 */
void
pmap_change_wiring(pmap, va, wired)
	register pmap_t	pmap;
	vm_offset_t	va;
	boolean_t	wired;
{
	register pt_entry_t *pte;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_change_wiring(%x, %x, %x)\n", pmap, va, wired);
#endif
	if (pmap == NULL)
		return;

	pte = pmap_pte(pmap, va);
#ifdef DEBUG
	/*
	 * Page table page is not allocated.
	 * Should this ever happen?  Ignore it for now,
	 * we don't want to force allocation of unnecessary PTE pages.
	 */
	if (!pmap_ste_v(pmap, va)) {
		if (pmapdebug & PDB_PARANOIA)
			printf("pmap_change_wiring: invalid STE for %x\n", va);
		return;
	}
	/*
	 * Page not valid.  Should this ever happen?
	 * Just continue and change wiring anyway.
	 */
	if (!pmap_pte_v(pte)) {
		if (pmapdebug & PDB_PARANOIA)
			printf("pmap_change_wiring: invalid PTE for %x\n", va);
	}
#endif
	if (wired && !pmap_pte_w(pte) || !wired && pmap_pte_w(pte)) {
		if (wired)
			pmap->pm_stats.wired_count++;
		else
			pmap->pm_stats.wired_count--;
	}
	/*
	 * Wiring is not a hardware characteristic so there is no need
	 * to invalidate TLB.
	 */
#if defined(DYNPGSIZE)
	{
		register int ix = 0;

		do {
			pmap_pte_set_w(pte++, wired);
		} while (++ix != lunapagesperpage);
	}
#else
	pmap_pte_set_w(pte, wired);
#endif
}

/*
 *	Routine:	pmap_extract
 *	Function:
 *		Extract the physical page address associated
 *		with the given map/virtual_address pair.
 */

vm_offset_t
pmap_extract(pmap, va)
	register pmap_t	pmap;
	vm_offset_t va;
{
	register vm_offset_t pa;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_extract(%x, %x) -> ", pmap, va);
#endif
	pa = 0;
	if (pmap && pmap_ste_v(pmap, va))
		pa = *(int *)pmap_pte(pmap, va);
	if (pa)
		pa = (pa & PG_FRAME) | (va & ~PG_FRAME);
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("%x\n", pa);
#endif
	return(pa);
}

/*
 *	Copy the range specified by src_addr/len
 *	from the source map to the range dst_addr/len
 *	in the destination map.
 *
 *	This routine is only advisory and need not do anything.
 */
void pmap_copy(dst_pmap, src_pmap, dst_addr, len, src_addr)
	pmap_t		dst_pmap;
	pmap_t		src_pmap;
	vm_offset_t	dst_addr;
	vm_size_t	len;
	vm_offset_t	src_addr;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_copy(%x, %x, %x, %x, %x)\n",
		       dst_pmap, src_pmap, dst_addr, len, src_addr);
#endif
}

/*
 *	Require that all active physical maps contain no
 *	incorrect entries NOW.  [This update includes
 *	forcing updates of any address map caching.]
 *
 *	Generally used to insure that a thread about
 *	to run will see a semantically correct world.
 */
void pmap_update()
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_update()\n");
#endif
	TBIA();
}

/*
 *	Routine:	pmap_collect
 *	Function:
 *		Garbage collects the physical map system for
 *		pages which are no longer used.
 *		Success need not be guaranteed -- that is, there
 *		may well be pages which are not referenced, but
 *		others may be collected.
 *	Usage:
 *		Called by the pageout daemon when pages are scarce.
 */
void
pmap_collect(pmap)
	pmap_t		pmap;
{
	register vm_offset_t pa;
	register pv_entry_t pv;
	register int *pte;
	vm_offset_t kpa;
	int s;

#ifdef DEBUG
	int *ste;
	int opmapdebug;
#endif
	if (pmap != kernel_pmap)
		return;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_collect(%x)\n", pmap);
	kpt_stats.collectscans++;
#endif
	s = splimp();
	for (pa = vm_first_phys; pa < vm_last_phys; pa += PAGE_SIZE) {
		register struct kpt_page *kpt, **pkpt;

		/*
		 * Locate physical pages which are being used as kernel
		 * page table pages.
		 */
		pv = pa_to_pvh(pa);
		if (pv->pv_pmap != kernel_pmap || !(pv->pv_flags & PV_PTPAGE))
			continue;
		do {
			if (pv->pv_ptste && pv->pv_ptpmap == kernel_pmap)
				break;
		} while (pv = pv->pv_next);
		if (pv == NULL)
			continue;
#ifdef DEBUG
		if (pv->pv_va < (vm_offset_t)Sysmap ||
		    pv->pv_va >= (vm_offset_t)Sysmap + LUNA_MAX_PTSIZE)
			printf("collect: kernel PT VA out of range\n");
		else
			goto ok;
		pmap_pvdump(pa);
		continue;
ok:
#endif
		pte = (int *)(pv->pv_va + LUNA_PAGE_SIZE);
		while (--pte >= (int *)pv->pv_va && *pte == PG_NV)
			;
		if (pte >= (int *)pv->pv_va)
			continue;

#ifdef DEBUG
		if (pmapdebug & (PDB_PTPAGE|PDB_COLLECT)) {
			printf("collect: freeing KPT page at %x (ste %x@%x)\n",
			       pv->pv_va, *(int *)pv->pv_ptste, pv->pv_ptste);
			opmapdebug = pmapdebug;
			pmapdebug |= PDB_PTPAGE;
		}

		ste = (int *)pv->pv_ptste;
#endif
		/*
		 * If all entries were invalid we can remove the page.
		 * We call pmap_remove to take care of invalidating ST
		 * and Sysptmap entries.
		 */
		kpa = pmap_extract(pmap, pv->pv_va);
		pmap_remove(pmap, pv->pv_va, pv->pv_va + LUNA_PAGE_SIZE);
		/*
		 * Use the physical address to locate the original
		 * (kmem_alloc assigned) address for the page and put
		 * that page back on the free list.
		 */
		for (pkpt = &kpt_used_list, kpt = *pkpt;
		     kpt != (struct kpt_page *)0;
		     pkpt = &kpt->kpt_next, kpt = *pkpt)
			if (kpt->kpt_pa == kpa)
				break;
#ifdef DEBUG
		if (kpt == (struct kpt_page *)0)
			panic("pmap_collect: lost a KPT page");
		if (pmapdebug & (PDB_PTPAGE|PDB_COLLECT))
			printf("collect: %x (%x) to free list\n",
			       kpt->kpt_va, kpa);
#endif
		*pkpt = kpt->kpt_next;
		kpt->kpt_next = kpt_free_list;
		kpt_free_list = kpt;
#ifdef DEBUG
		kpt_stats.kptinuse--;
		kpt_stats.collectpages++;
		if (pmapdebug & (PDB_PTPAGE|PDB_COLLECT))
			pmapdebug = opmapdebug;

		if (*ste)
			printf("collect: kernel STE at %x still valid (%x)\n",
			       ste, *ste);
		ste = (int *)&Sysptmap[(st_entry_t *)ste-pmap_ste(kernel_pmap, 0)];
		if (*ste)
			printf("collect: kernel PTmap at %x still valid (%x)\n",
			       ste, *ste);
#endif
	}
	splx(s);
}

void
pmap_activate(pmap, pcbp)
	register pmap_t pmap;
	struct pcb *pcbp;
{
#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_SEGTAB))
		printf("pmap_activate(%x, %x)\n", pmap, pcbp);
#endif
	PMAP_ACTIVATE(pmap, pcbp, pmap == curproc->p_vmspace->vm_map.pmap);
}

/*
 *	pmap_zero_page zeros the specified (machine independent)
 *	page by mapping the page into virtual memory and using
 *	bzero to clear its contents, one machine dependent page
 *	at a time.
 *
 *	XXX this is a bad implementation for virtual cache machines
 *	(320/350) because pmap_enter doesn't cache-inhibit the temporary
 *	kernel mapping and we wind up with data cached for that KVA.
 *	It is probably a win for physical cache machines (370/380)
 *	as the cache loading is not wasted.
 */
void
pmap_zero_page(phys)
	vm_offset_t phys;
{
	register vm_offset_t kva;
	extern caddr_t CADDR1;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_zero_page(%x)\n", phys);
#endif
	kva = (vm_offset_t) CADDR1;
#if defined(DYNPGSIZE)
	{
		register int ix = 0;

		do {
			pmap_enter(kernel_pmap, kva, phys,
				   VM_PROT_READ|VM_PROT_WRITE, TRUE);
			bzero((caddr_t)kva, LUNA_PAGE_SIZE);
			pmap_remove(kernel_pmap, kva, kva+LUNA_PAGE_SIZE);
			phys += LUNA_PAGE_SIZE;
		} while (++ix != lunapagesperpage);
	}
#else
	pmap_enter(kernel_pmap, kva, phys, VM_PROT_READ|VM_PROT_WRITE, TRUE);
	bzero((caddr_t)kva, LUNA_PAGE_SIZE);
	pmap_remove(kernel_pmap, kva, kva+PAGE_SIZE);
#endif
}

/*
 *	pmap_copy_page copies the specified (machine independent)
 *	page by mapping the page into virtual memory and using
 *	bcopy to copy the page, one machine dependent page at a
 *	time.
 *
 *
 *	XXX this is a bad implementation for virtual cache machines
 *	(320/350) because pmap_enter doesn't cache-inhibit the temporary
 *	kernel mapping and we wind up with data cached for that KVA.
 *	It is probably a win for physical cache machines (370/380)
 *	as the cache loading is not wasted.
 */
void
pmap_copy_page(src, dst)
	vm_offset_t src, dst;
{
	register vm_offset_t skva, dkva;
	extern caddr_t CADDR1, CADDR2;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_copy_page(%x, %x)\n", src, dst);
#endif
	skva = (vm_offset_t) CADDR1;
	dkva = (vm_offset_t) CADDR2;
#if defined(DYNPGSIZE)
	{
		register int ix = 0;

		do {
			pmap_enter(kernel_pmap, skva, src, VM_PROT_READ, TRUE);
			pmap_enter(kernel_pmap, dkva, dst,
				   VM_PROT_READ|VM_PROT_WRITE, TRUE);
			bcopy((caddr_t)skva, (caddr_t)dkva, PAGE_SIZE);
			/* CADDR1 and CADDR2 are virtually contiguous */
			pmap_remove(kernel_pmap, skva, skva+2*LUNA_PAGE_SIZE);
			src += LUNA_PAGE_SIZE;
			dst += LUNA_PAGE_SIZE;
		} while (++ix != lunapagesperpage);
	}
#else
	pmap_enter(kernel_pmap, skva, src, VM_PROT_READ, TRUE);
	pmap_enter(kernel_pmap, dkva, dst, VM_PROT_READ|VM_PROT_WRITE, TRUE);
	bcopy((caddr_t)skva, (caddr_t)dkva, PAGE_SIZE);
	/* CADDR1 and CADDR2 are virtually contiguous */
	pmap_remove(kernel_pmap, skva, skva+2*PAGE_SIZE);
#endif
}

/*
 *	Routine:	pmap_pageable
 *	Function:
 *		Make the specified pages (by pmap, offset)
 *		pageable (or not) as requested.
 *
 *		A page which is not pageable may not take
 *		a fault; therefore, its page table entry
 *		must remain valid for the duration.
 *
 *		This routine is merely advisory; pmap_enter
 *		will specify that these pages are to be wired
 *		down (or not) as appropriate.
 */
void
pmap_pageable(pmap, sva, eva, pageable)
	pmap_t		pmap;
	vm_offset_t	sva, eva;
	boolean_t	pageable;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_pageable(%x, %x, %x, %x)\n",
		       pmap, sva, eva, pageable);
#endif
	/*
	 * If we are making a PT page pageable then all valid
	 * mappings must be gone from that page.  Hence it should
	 * be all zeros and there is no need to clean it.
	 * Assumptions:
	 *	- we are called with only one page at a time
	 *	- PT pages have only one pv_table entry
	 */
	if (pmap == kernel_pmap && pageable && sva + PAGE_SIZE == eva) {
		register pv_entry_t pv;
		register vm_offset_t pa;

#ifdef DEBUG
		if ((pmapdebug & (PDB_FOLLOW|PDB_PTPAGE)) == PDB_PTPAGE)
			printf("pmap_pageable(%x, %x, %x, %x)\n",
			       pmap, sva, eva, pageable);
#endif
		if (!pmap_ste_v(pmap, sva))
			return;
		pa = pmap_pte_pa(pmap_pte(pmap, sva));
		if (pa < vm_first_phys || pa >= vm_last_phys)
			return;
		pv = pa_to_pvh(pa);
		if (pv->pv_ptste == NULL)
			return;
#ifdef DEBUG
		if (pv->pv_va != sva || pv->pv_next) {
			printf("pmap_pageable: bad PT page va %x next %x\n",
			       pv->pv_va, pv->pv_next);
			return;
		}
#endif
		/*
		 * Mark it unmodified to avoid pageout
		 */
		pmap_changebit(pa, PG_M, FALSE);
#ifdef DEBUG
		if (pmapdebug & PDB_PTPAGE)
			printf("pmap_pageable: PT page %x(%x) unmodified\n",
			       sva, *(int *)pmap_pte(pmap, sva));
		if (pmapdebug & PDB_WIRING)
			pmap_check_wiring("pageable", sva);
#endif
	}
}

/*
 *	Clear the modify bits on the specified physical page.
 */

void
pmap_clear_modify(pa)
	vm_offset_t	pa;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_clear_modify(%x)\n", pa);
#endif
	pmap_changebit(pa, PG_M, FALSE);
}

/*
 *	pmap_clear_reference:
 *
 *	Clear the reference bit on the specified physical page.
 */

void pmap_clear_reference(pa)
	vm_offset_t	pa;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_clear_reference(%x)\n", pa);
#endif
	pmap_changebit(pa, PG_U, FALSE);
}

/*
 *	pmap_is_referenced:
 *
 *	Return whether or not the specified physical page is referenced
 *	by any physical maps.
 */

boolean_t
pmap_is_referenced(pa)
	vm_offset_t	pa;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW) {
		boolean_t rv = pmap_testbit(pa, PG_U);
		printf("pmap_is_referenced(%x) -> %c\n", pa, "FT"[rv]);
		return(rv);
	}
#endif
	return(pmap_testbit(pa, PG_U));
}

/*
 *	pmap_is_modified:
 *
 *	Return whether or not the specified physical page is modified
 *	by any physical maps.
 */

boolean_t
pmap_is_modified(pa)
	vm_offset_t	pa;
{
#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW) {
		boolean_t rv = pmap_testbit(pa, PG_M);
		printf("pmap_is_modified(%x) -> %c\n", pa, "FT"[rv]);
		return(rv);
	}
#endif
	return(pmap_testbit(pa, PG_M));
}

vm_offset_t
pmap_phys_address(ppn)
	int ppn;
{
	return(luna_ptob(ppn));
}

/*
 * Miscellaneous support routines follow
 */

/* static */
boolean_t
pmap_testbit(pa, bit)
	register vm_offset_t pa;
	int bit;
{
	register pv_entry_t pv;
	register int *pte;
	int s;

	if (pa < vm_first_phys || pa >= vm_last_phys)
		return(FALSE);

	pv = pa_to_pvh(pa);
	s = splimp();
	/*
	 * Check saved info first
	 */
	if (pmap_attributes[pa_index(pa)] & bit) {
		splx(s);
		return(TRUE);
	}
	/*
	 * Not found, check current mappings returning
	 * immediately if found.
	 */
	if (pv->pv_pmap != NULL) {
		for (; pv; pv = pv->pv_next) {
			pte = (int *) pmap_pte(pv->pv_pmap, pv->pv_va);
#if defined(DYNPGSIZE)
			{
				register int ix = 0;

				do {
					if (*pte++ & bit) {
						splx(s);
						return(TRUE);
					}
				} while (++ix != lunapagesperpage);
			}
#else
			if (*pte & bit) {
				splx(s);
				return(TRUE);
			}
#endif
		}
	}
	splx(s);
	return(FALSE);
}

/* static */
pmap_changebit(pa, bit, setem)
	register vm_offset_t pa;
	int bit;
	boolean_t setem;
{
	register pv_entry_t pv;
	register int *pte, npte;
	vm_offset_t va;
	int s;
	boolean_t firstpage = TRUE;

#ifdef DEBUG
	if (pmapdebug & PDB_BITS)
		printf("pmap_changebit(%x, %x, %s)\n",
		       pa, bit, setem ? "set" : "clear");
#endif
	if (pa < vm_first_phys || pa >= vm_last_phys)
		return;

	pv = pa_to_pvh(pa);
	s = splimp();
	/*
	 * Clear saved attributes (modify, reference)
	 */
	if (!setem)
		pmap_attributes[pa_index(pa)] &= ~bit;
	/*
	 * Loop over all current mappings setting/clearing as appropos
	 * If setting RO do we need to clear the VAC?
	 */
	if (pv->pv_pmap != NULL) {
#ifdef DEBUG
		int toflush = 0;
#endif
		for (; pv; pv = pv->pv_next) {
#ifdef DEBUG
			toflush |= (pv->pv_pmap == kernel_pmap) ? 2 : 1;
#endif
			va = pv->pv_va;

			/*
			 * XXX don't write protect pager mappings
			 */
			if (bit == PG_RO) {
				extern vm_offset_t pager_sva, pager_eva;

				if (va >= pager_sva && va < pager_eva)
					continue;
			}

			pte = (int *) pmap_pte(pv->pv_pmap, va);
#if defined(DYNPGSIZE)
			{
				register int ix = 0;

				do {
					if (setem)
						npte = *pte | bit;
					else
						npte = *pte & ~bit;
					if (*pte != npte) {
						*pte = npte;
						TBIS(va);
					}
					va += LUNA_PAGE_SIZE;
					pte++;
				} while (++ix != lunapagesperpage);
			}
#else
			if (setem)
				npte = *pte | bit;
			else
				npte = *pte & ~bit;
			if (*pte != npte) {
				*pte = npte;
				TBIS(va);
			}
#endif
		}
	}
	splx(s);
}

/* static */
void
pmap_enter_ptpage(pmap, va)
	register pmap_t pmap;
	register vm_offset_t va;
{
	register vm_offset_t ptpa;
	register pv_entry_t pv;
	st_entry_t *ste;
	int s;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_ENTER|PDB_PTPAGE))
		printf("pmap_enter_ptpage: pmap %x, va %x\n", pmap, va);
	enter_stats.ptpneeded++;
#endif
	/*
	 * Allocate a segment table if necessary.  Note that it is allocated
	 * from kernel_map and not pt_map.  This keeps user page tables
	 * aligned on segment boundaries in the kernel address space.
	 * The segment table is wired down.  It will be freed whenever the
	 * reference count drops to zero.
	 */
	if (pmap->pm_stab == Segtabzero) {
		pmap->pm_stab = (st_entry_t *)
			kmem_alloc(kernel_map, LUNA_STSIZE);
		pmap->pm_stpa = (st_entry_t *)
			pmap_extract(kernel_pmap, (vm_offset_t)pmap->pm_stab);
		pmap->pm_stchanged = TRUE;
		/*
		 * XXX may have changed segment table pointer for current
		 * process so update now to reload hardware.
		 */
		if (pmap == curproc->p_vmspace->vm_map.pmap)
			PMAP_ACTIVATE(pmap, (struct pcb *)curproc->p_addr, 1);
#ifdef DEBUG
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE|PDB_SEGTAB))
			printf("enter: pmap %x stab %x(%x)\n",
			       pmap, pmap->pm_stab, pmap->pm_stpa);
#endif
	}

	ste = pmap_ste(pmap, va);
	va = trunc_page((vm_offset_t)pmap_pte(pmap, va));

	/*
	 * In the kernel we allocate a page from the kernel PT page
	 * free list and map it into the kernel page table map (via
	 * pmap_enter).
	 */
	if (pmap == kernel_pmap) {
		register struct kpt_page *kpt;

		s = splimp();
		if ((kpt = kpt_free_list) == (struct kpt_page *)0) {
			/*
			 * No PT pages available.
			 * Try once to free up unused ones.
			 */
#ifdef DEBUG
			if (pmapdebug & PDB_COLLECT)
				printf("enter: no KPT pages, collecting...\n");
#endif
			pmap_collect(kernel_pmap);
			if ((kpt = kpt_free_list) == (struct kpt_page *)0)
				panic("pmap_enter_ptpage: can't get KPT page");
		}
#ifdef DEBUG
		if (++kpt_stats.kptinuse > kpt_stats.kptmaxuse)
			kpt_stats.kptmaxuse = kpt_stats.kptinuse;
#endif
		kpt_free_list = kpt->kpt_next;
		kpt->kpt_next = kpt_used_list;
		kpt_used_list = kpt;
		ptpa = kpt->kpt_pa;
		bzero((caddr_t)kpt->kpt_va, LUNA_PAGE_SIZE);
		pmap_enter(pmap, va, ptpa, VM_PROT_DEFAULT, TRUE);
#ifdef DEBUG
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE)) {
			int ix = pmap_ste(pmap, va) - pmap_ste(pmap, 0);

			printf("enter: add &Sysptmap[%d]: %x (KPT page %x)\n",
			       ix, *(int *)&Sysptmap[ix], kpt->kpt_va);
		}
#endif
		splx(s);
	}
	/*
	 * For user processes we just simulate a fault on that location
	 * letting the VM system allocate a zero-filled page.
	 */
	else {
#ifdef DEBUG
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE))
			printf("enter: about to fault UPT pg at %x\n", va);
		s = vm_fault(pt_map, va, VM_PROT_READ|VM_PROT_WRITE, FALSE);
		if (s != KERN_SUCCESS) {
			printf("vm_fault(pt_map, %x, RW, 0) -> %d\n", va, s);
			panic("pmap_enter: vm_fault failed");
		}
#else
		if (vm_fault(pt_map, va, VM_PROT_READ|VM_PROT_WRITE, FALSE)
		    != KERN_SUCCESS)
			panic("pmap_enter: vm_fault failed");
#endif
		ptpa = pmap_extract(kernel_pmap, va);
#ifdef DEBUG
		PHYS_TO_VM_PAGE(ptpa)->ptpage = TRUE;
#endif
	}
	/*
	 * Locate the PV entry in the kernel for this PT page and
	 * record the STE address.  This is so that we can invalidate
	 * the STE when we remove the mapping for the page.
	 */
	pv = pa_to_pvh(ptpa);
	s = splimp();
	if (pv) {
		pv->pv_flags |= PV_PTPAGE;
		do {
			if (pv->pv_pmap == kernel_pmap && pv->pv_va == va)
				break;
		} while (pv = pv->pv_next);
	}
#ifdef DEBUG
	if (pv == NULL)
		panic("pmap_enter_ptpage: PT page not entered");
#endif
	pv->pv_ptste = ste;
	pv->pv_ptpmap = pmap;
#ifdef DEBUG
	if (pmapdebug & (PDB_ENTER|PDB_PTPAGE))
		printf("enter: new PT page at PA %x, ste at %x\n", ptpa, ste);
#endif

	/*
	 * Map the new PT page into the segment table.
	 * Also increment the reference count on the segment table if this
	 * was a user page table page.  Note that we don't use vm_map_pageable
	 * to keep the count like we do for PT pages, this is mostly because
	 * it would be difficult to identify ST pages in pmap_pageable to
	 * release them.  We also avoid the overhead of vm_map_pageable.
	 */
	*(int *)ste = (ptpa & SG_FRAME) | SG_RW | SG_V;
	if (pmap != kernel_pmap) {
		pmap->pm_sref++;
#ifdef DEBUG
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE|PDB_SEGTAB))
			printf("enter: stab %x refcnt %d\n",
			       pmap->pm_stab, pmap->pm_sref);
#endif
	}
	/*
	 * Flush stale TLB info.
	 */
	if (pmap == kernel_pmap)
		TBIAS();
	else
		TBIAU();
	pmap->pm_ptpages++;
	splx(s);
}

#ifdef DEBUG
pmap_pvdump(pa)
	vm_offset_t pa;
{
	register pv_entry_t pv;

	printf("pa %x", pa);
	for (pv = pa_to_pvh(pa); pv; pv = pv->pv_next)
		printf(" -> pmap %x, va %x, ptste %x, ptpmap %x, flags %x",
		       pv->pv_pmap, pv->pv_va, pv->pv_ptste, pv->pv_ptpmap,
		       pv->pv_flags);
	printf("\n");
}

pmap_check_wiring(str, va)
	char *str;
	vm_offset_t va;
{
	vm_map_entry_t entry;
	register int count, *pte;

	va = trunc_page(va);
	if (!pmap_ste_v(kernel_pmap, va) ||
	    !pmap_pte_v(pmap_pte(kernel_pmap, va)))
		return;

	if (!vm_map_lookup_entry(pt_map, va, &entry)) {
		printf("wired_check: entry for %x not found\n", va);
		return;
	}
	count = 0;
	for (pte = (int *)va; pte < (int *)(va+PAGE_SIZE); pte++)
		if (*pte)
			count++;
	if (entry->wired_count != count)
		printf("*%s*: %x: w%d/a%d\n",
		       str, va, entry->wired_count, count);
}
#endif
