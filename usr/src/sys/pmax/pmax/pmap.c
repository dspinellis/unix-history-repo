/* 
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmap.c	7.11 (Berkeley) %G%
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

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/malloc.h>
#include <sys/user.h>

#include <vm/vm.h>
#include <vm/vm_kern.h>
#include <vm/vm_page.h>

#include <machine/machConst.h>
#include <machine/pte.h>

/*
 * For each vm_page_t, there is a list of all currently valid virtual
 * mappings of that page.  An entry is a pv_entry_t, the list is pv_table.
 * XXX really should do this as a part of the higher level code.
 */
typedef struct pv_entry {
	struct pv_entry	*pv_next;	/* next pv_entry */
	struct pmap	*pv_pmap;	/* pmap where mapping lies */
	vm_offset_t	pv_va;		/* virtual address for mapping */
} *pv_entry_t;

pv_entry_t	pv_table;	/* array of entries, one per page */
extern void	pmap_remove_pv();

#define pa_index(pa)		atop((pa) - first_phys_addr)
#define pa_to_pvh(pa)		(&pv_table[pa_index(pa)])

#ifdef DEBUG
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
	int cachehit;	/* new entry forced valid entry out */
} enter_stats;
struct {
	int calls;
	int removes;
	int flushes;
	int pidflushes;	/* HW pid stolen */
	int pvfirst;
	int pvsearch;
} remove_stats;

int pmapdebug;
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
#define PDB_TLBPID	0x0400
#define PDB_PARANOIA	0x2000
#define PDB_WIRING	0x4000
#define PDB_PVDUMP	0x8000

#endif /* DEBUG */

u_int	whichpids[2] = {	/* bit mask of hardware PID's in use */
	3, 0
};

struct pmap	kernel_pmap_store;
pmap_t		cur_pmap;	/* current pmap mapped in hardware */

vm_offset_t    	avail_start;	/* PA of first available physical page */
vm_offset_t	avail_end;	/* PA of last available physical page */
vm_size_t	mem_size;	/* memory size in bytes */
vm_offset_t	virtual_avail;  /* VA of first avail page (after kernel bss)*/
vm_offset_t	virtual_end;	/* VA of last avail page (end of kernel AS) */
int		pmaxpagesperpage;	/* PAGE_SIZE / NBPG */
#ifdef ATTR
char		*pmap_attributes;	/* reference and modify bits */
#endif
pmap_hash_t	zero_pmap_hash;		/* empty TLB hash table for init */

/*
 *	Bootstrap the system enough to run with virtual memory.
 */
void
pmap_bootstrap(firstaddr)
	vm_offset_t firstaddr;
{
	register int i;
	vm_offset_t start = firstaddr;
	extern int maxmem, physmem;

	/*
	 * Allocate a TLB hash table for the kernel.
	 * This could be a KSEG0 address and thus save TLB entries but
	 * its faster and simpler in assembly language to have a
	 * fixed address that can be accessed with a 16 bit signed offset.
	 * Note: the kernel pm_hash field is null, user pm_hash fields are
	 * either the table or zero_pmap_hash.
	 */
	kernel_pmap_store.pm_hash = (pmap_hash_t)0;
	for (i = 0; i < PMAP_HASH_KPAGES; i++) {
		MachTLBWriteIndexed(i + UPAGES + PMAP_HASH_UPAGES,
			PMAP_HASH_KADDR + (i << PGSHIFT),
			firstaddr | PG_V | PG_M | PG_G);
		firstaddr += NBPG;
	}

	/*
	 * Allocate an empty TLB hash table for initial pmap's.
	 */
	zero_pmap_hash = (pmap_hash_t)MACH_PHYS_TO_CACHED(firstaddr);
 
	/* init proc[0]'s pmap hash table */
	for (i = 0; i < PMAP_HASH_UPAGES; i++) {
		kernel_pmap_store.pm_hash_ptes[i] = firstaddr | PG_V | PG_RO;
		MachTLBWriteIndexed(i + UPAGES,
			(PMAP_HASH_UADDR + (i << PGSHIFT)) |
				(1 << VMMACH_TLB_PID_SHIFT),
			kernel_pmap_store.pm_hash_ptes[i]);
		firstaddr += NBPG;
	}

	/*
	 * Allocate memory for pv_table.
	 * This will allocate more entries than we really need.
	 * We should do this in pmap_init when we know the actual
	 * phys_start and phys_end but its better to use phys addresses
	 * rather than kernel virtual addresses mapped through the TLB.
	 */
	i = (maxmem - pmax_btop(firstaddr)) * sizeof(struct pv_entry);
	i = pmax_round_page(i);
	pv_table = (pv_entry_t)MACH_PHYS_TO_CACHED(firstaddr);
	firstaddr += i;

	/*
	 * Clear allocated memory.
	 */
	bzero((caddr_t)MACH_PHYS_TO_CACHED(start), firstaddr - start);

	avail_start = firstaddr;
	avail_end = pmax_ptob(maxmem);
	mem_size = avail_end - avail_start;

	virtual_avail = VM_MIN_KERNEL_ADDRESS;
	virtual_end = VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES * NPTEPG * NBPG;
	/* XXX need to decide how to set cnt.v_page_size */
	pmaxpagesperpage = 1;

	cur_pmap = &kernel_pmap_store;
	simple_lock_init(&kernel_pmap_store.pm_lock);
	kernel_pmap_store.pm_count = 1;
}

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
pmap_bootstrap_alloc(size)
	int size;
{
	vm_offset_t val;
	extern boolean_t vm_page_startup_initialized;

	if (vm_page_startup_initialized)
		panic("pmap_bootstrap_alloc: called after startup initialized");

	val = MACH_PHYS_TO_CACHED(avail_start);
	size = round_page(size);
	avail_start += size;

	blkclr((caddr_t)val, size);
	return ((void *)val);
}

/*
 *	Initialize the pmap module.
 *	Called by vm_init, to initialize any structures that the pmap
 *	system needs to map virtual memory.
 */
void
pmap_init(phys_start, phys_end)
	vm_offset_t phys_start, phys_end;
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_init(%x, %x)\n", phys_start, phys_end);
#endif
}

/*
 *	Used to map a range of physical addresses into kernel
 *	virtual address space.
 *
 *	This routine should only be called by vm_page_startup()
 *	with KSEG0 addresses.
 */
vm_offset_t
pmap_map(virt, start, end, prot)
	vm_offset_t virt;
	vm_offset_t start;
	vm_offset_t end;
	int prot;
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_map(%x, %x, %x, %x)\n", virt, start, end, prot);
#endif

	return (round_page(end));
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
	vm_size_t size;
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
		return (NULL);

	printf("pmap_create(%x) XXX\n", size); /* XXX */
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
	register int i;
	extern struct vmspace vmspace0;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_CREATE))
		printf("pmap_pinit(%x)\n", pmap);
#endif
	simple_lock_init(&pmap->pm_lock);
	pmap->pm_count = 1;
	pmap->pm_flags = 0;
	pmap->pm_hash = zero_pmap_hash;
	for (i = 0; i < PMAP_HASH_UPAGES; i++)
		pmap->pm_hash_ptes[i] =
			(MACH_CACHED_TO_PHYS(zero_pmap_hash) + (i << PGSHIFT)) |
				PG_V | PG_RO;
	if (pmap == &vmspace0.vm_pmap)
		pmap->pm_tlbpid = 1;	/* preallocated in mach_init() */
	else
		pmap->pm_tlbpid = -1;	/* none allocated yet */
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

	printf("pmap_destroy(%x) XXX\n", pmap); /* XXX */
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
	register pmap_t pmap;
{
	register int id;
#ifdef DIAGNOSTIC
	register int i;
#endif

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_release(%x)\n", pmap);
#endif

	if (pmap->pm_hash && pmap->pm_hash != zero_pmap_hash) {
		kmem_free(kernel_map, (vm_offset_t)pmap->pm_hash,
			PMAP_HASH_SIZE);
		pmap->pm_hash = zero_pmap_hash;
	}
	if ((id = pmap->pm_tlbpid) < 0)
		return;
#ifdef DIAGNOSTIC
	if (!(whichpids[id >> 5] & (1 << (id & 0x1F))))
		panic("pmap_release: id free");
#endif
	MachTLBFlushPID(id);
	whichpids[id >> 5] &= ~(1 << (id & 0x1F));
	pmap->pm_flags &= ~PM_MODIFIED;
	pmap->pm_tlbpid = -1;
	if (pmap == cur_pmap)
		cur_pmap = (pmap_t)0;
#ifdef DIAGNOSTIC
	/* invalidate user PTE cache */
	for (i = 0; i < PMAP_HASH_UPAGES; i++)
		MachTLBWriteIndexed(i + UPAGES, MACH_RESERVED_ADDR, 0);
#endif
}

/*
 *	Add a reference to the specified pmap.
 */
void
pmap_reference(pmap)
	pmap_t pmap;
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
	register vm_offset_t va;
	register pv_entry_t pv, npv;
	register int i;
	pmap_hash_t hp;
	unsigned entry;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_REMOVE|PDB_PROTECT))
		printf("pmap_remove(%x, %x, %x)\n", pmap, sva, eva);
	remove_stats.calls++;
#endif
	if (pmap == NULL)
		return;

	/* anything in the cache? */
	if (pmap->pm_tlbpid < 0 || pmap->pm_hash == zero_pmap_hash)
		return;

	if (!pmap->pm_hash) {
		register pt_entry_t *pte;

		/* remove entries from kernel pmap */
#ifdef DIAGNOSTIC
		if (sva < VM_MIN_KERNEL_ADDRESS ||
		    eva > VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES*NPTEPG*NBPG)
			panic("pmap_remove");
#endif
		pte = kvtopte(sva);
		for (va = sva; va < eva; va += NBPG, pte++) {
			entry = pte->pt_entry;
			if (!(entry & PG_V))
				continue;
			if (entry & PG_WIRED)
				pmap->pm_stats.wired_count--;
			pmap->pm_stats.resident_count--;
			pmap_remove_pv(pmap, va, entry & PG_FRAME);
#ifdef ATTR
			pmap_attributes[atop(entry - KERNBASE)] = 0;
#endif
			pte->pt_entry = PG_NV;
			/*
			 * Flush the TLB for the given address.
			 */
			MachTLBFlushAddr(va);
#ifdef DEBUG
			remove_stats.flushes++;
#endif
		}
		return;
	}

	va = sva | (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
	eva |= (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
	/*
	 * If we are not in the current address space, just flush the
	 * software cache and not the hardware.
	 */
	if (pmap != cur_pmap) {
		for (; va < eva; va += NBPG) {
			hp = &pmap->pm_hash[PMAP_HASH(va)];
			if (hp->pmh_pte[0].high == va)
				i = 0;
			else if (hp->pmh_pte[1].high == va)
				i = 1;
			else
				continue;

			hp->pmh_pte[i].high = 0;
			entry = hp->pmh_pte[i].low;
			if (entry & PG_WIRED)
				pmap->pm_stats.wired_count--;
			pmap->pm_stats.resident_count--;
			pmap_remove_pv(pmap, va & PG_FRAME, entry & PG_FRAME);
#ifdef ATTR
			pmap_attributes[atop(entry - KERNBASE)] = 0;
#endif
			pmap->pm_flags |= PM_MODIFIED;
#ifdef DEBUG
			remove_stats.removes++;
#endif
		}
		return;
	}

	for (; va < eva; va += NBPG) {
		hp = &pmap->pm_hash[PMAP_HASH(va)];
		if (hp->pmh_pte[0].high == va)
			i = 0;
		else if (hp->pmh_pte[1].high == va)
			i = 1;
		else
			continue;

		hp->pmh_pte[i].high = 0;
		entry = hp->pmh_pte[i].low;
		if (entry & PG_WIRED)
			pmap->pm_stats.wired_count--;
		pmap->pm_stats.resident_count--;
		pmap_remove_pv(pmap, va & PG_FRAME, entry & PG_FRAME);
#ifdef ATTR
		pmap_attributes[atop(entry - KERNBASE)] = 0;
#endif
		/*
		* Flush the TLB for the given address.
		*/
		MachTLBFlushAddr(va);
#ifdef DEBUG
		remove_stats.flushes++;
#endif
	}
}

/*
 *	pmap_page_protect:
 *
 *	Lower the permission for all mappings to a given page.
 */
void
pmap_page_protect(pa, prot)
	vm_offset_t pa;
	vm_prot_t prot;
{
	register pv_entry_t pv;
	register vm_offset_t va;
	int s;

#ifdef DEBUG
	if ((pmapdebug & (PDB_FOLLOW|PDB_PROTECT)) ||
	    prot == VM_PROT_NONE && (pmapdebug & PDB_REMOVE))
		printf("pmap_page_protect(%x, %x)\n", pa, prot);
#endif
	if (!IS_VM_PHYSADDR(pa))
		return;

	switch (prot) {
	case VM_PROT_ALL:
		break;

	/* copy_on_write */
	case VM_PROT_READ:
	case VM_PROT_READ|VM_PROT_EXECUTE:
		pv = pa_to_pvh(pa);
		s = splimp();
		/*
		 * Loop over all current mappings setting/clearing as appropos.
		 */
		if (pv->pv_pmap != NULL) {
			for (; pv; pv = pv->pv_next) {
				extern vm_offset_t pager_sva, pager_eva;
				va = pv->pv_va;

				/*
				 * XXX don't write protect pager mappings
				 */
				if (va >= pager_sva && va < pager_eva)
					continue;
				pmap_protect(pv->pv_pmap, va, va + PAGE_SIZE,
					prot);
			}
		}
		splx(s);
		break;

	/* remove_all */
	default:
		pv = pa_to_pvh(pa);
		s = splimp();
		while (pv->pv_pmap != NULL) {
			pmap_remove(pv->pv_pmap, pv->pv_va,
				    pv->pv_va + PAGE_SIZE);
		}
		splx(s);
	}
}

/*
 *	Set the physical protection on the
 *	specified range of this map as requested.
 */
void
pmap_protect(pmap, sva, eva, prot)
	register pmap_t pmap;
	vm_offset_t sva, eva;
	vm_prot_t prot;
{
	register vm_offset_t va;
	register int i;
	pmap_hash_t hp;
	u_int p;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_PROTECT))
		printf("pmap_protect(%x, %x, %x, %x)\n", pmap, sva, eva, prot);
#endif
	if (pmap == NULL)
		return;

	/* anything in the software cache? */
	if (pmap->pm_tlbpid < 0 || pmap->pm_hash == zero_pmap_hash)
		return;

	if (!(prot & VM_PROT_READ)) {
		pmap_remove(pmap, sva, eva);
		return;
	}

	if (!pmap->pm_hash) {
		register pt_entry_t *pte;

		/*
		 * Change entries in kernel pmap.
		 * This will trap if the page is writeable (in order to set
		 * the dirty bit) even if the dirty bit is already set. The
		 * optimization isn't worth the effort since this code isn't
		 * executed much. The common case is to make a user page
		 * read-only.
		 */
#ifdef DIAGNOSTIC
		if (sva < VM_MIN_KERNEL_ADDRESS ||
		    eva > VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES*NPTEPG*NBPG)
			panic("pmap_protect");
#endif
		p = (prot & VM_PROT_WRITE) ? PG_RW : PG_RO;
		pte = kvtopte(sva);
		for (va = sva; va < eva; va += NBPG, pte++) {
			if (!(pte->pt_entry & PG_V))
				continue;
			pte->pt_entry = (pte->pt_entry & ~(PG_M | PG_RO)) | p;
			/*
			 * Update the TLB if the given address is in the cache.
			 */
			MachTLBUpdate(va, pte->pt_entry);
		}
		return;
	}

	p = (prot & VM_PROT_WRITE) ? PG_RW : PG_RO;
	va = sva | (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
	eva |= (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
	/*
	 * If we are not in the current address space, just flush the
	 * software cache and not the hardware.
	 */
	if (pmap != cur_pmap) {
		for (; va < eva; va += NBPG) {
			hp = &pmap->pm_hash[PMAP_HASH(va)];
			if (hp->pmh_pte[0].high == va)
				i = 0;
			else if (hp->pmh_pte[1].high == va)
				i = 1;
			else
				continue;

			hp->pmh_pte[i].low = (hp->pmh_pte[i].low & ~(PG_M | PG_RO)) | p;
			pmap->pm_flags |= PM_MODIFIED;
		}
		return;
	}

	for (; va < eva; va += NBPG) {
		hp = &pmap->pm_hash[PMAP_HASH(va)];
		if (hp->pmh_pte[0].high == va)
			i = 0;
		else if (hp->pmh_pte[1].high == va)
			i = 1;
		else
			continue;

		hp->pmh_pte[i].low = (hp->pmh_pte[i].low & ~(PG_M | PG_RO)) | p;
		/*
		* Update the TLB if the given address is in the cache.
		*/
		MachTLBUpdate(hp->pmh_pte[i].high, hp->pmh_pte[i].low);
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
	register pmap_hash_t hp;
	register u_int npte;
	register int i, j;
	int newpos;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_ENTER))
		printf("pmap_enter(%x, %x, %x, %x, %x)\n",
		       pmap, va, pa, prot, wired);
#endif
#ifdef DIAGNOSTIC
	if (!pmap)
		panic("pmap_enter: pmap");
	if (pmap->pm_tlbpid < 0)
		panic("pmap_enter: tlbpid");
	if (!pmap->pm_hash) {
		enter_stats.kernel++;
		if (va < VM_MIN_KERNEL_ADDRESS ||
		    va >= VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES*NPTEPG*NBPG)
			panic("pmap_enter: kva");
	} else {
		enter_stats.user++;
		if (va & 0x80000000)
			panic("pmap_enter: uva");
	}
	if (pa & 0x80000000)
		panic("pmap_enter: pa");
	if (!(prot & VM_PROT_READ))
		panic("pmap_enter: prot");
#endif

	/*
	 * See if we need to create a new TLB cache.
	 */
	if (pmap->pm_hash == zero_pmap_hash) {
		register vm_offset_t kva;
		register pt_entry_t *pte;

		kva = kmem_alloc(kernel_map, PMAP_HASH_SIZE);
		pmap->pm_hash = (pmap_hash_t)kva;

		/*
		 * Convert the kernel virtual address to a physical one
		 * and cache it in the pmap. Note: if the phyical address
		 * can change (due to memory compaction in kmem_alloc?),
		 * we will have to update things.
		 */
		pte = kvtopte(kva);
		for (i = 0; i < PMAP_HASH_UPAGES; i++) {
			pmap->pm_hash_ptes[i] = pte->pt_entry & ~PG_G;
			pte++;
		}

		/*
		 * Map in new TLB cache if it is current.
		 */
		if (pmap == cur_pmap) {
			for (i = 0; i < PMAP_HASH_UPAGES; i++) {
				MachTLBWriteIndexed(i + UPAGES,
					(PMAP_HASH_UADDR + (i << PGSHIFT)) |
						(pmap->pm_tlbpid  <<
						VMMACH_TLB_PID_SHIFT),
					pmap->pm_hash_ptes[i]);
			}
		}
#ifdef DIAGNOSTIC
		for (i = 0; i < PAGE_SIZE; i += sizeof(int), kva += sizeof(int))
			if (*(int *)kva != 0)
				panic("pmap_enter: *kva != 0");
#endif
	}

	if (IS_VM_PHYSADDR(pa)) {
		register pv_entry_t pv, npv;
		int s;

		if (!(prot & VM_PROT_WRITE))
			npte = PG_RO;
		else {
			register vm_page_t mem;

			mem = PHYS_TO_VM_PAGE(pa);
			if ((int)va < 0) {
				/*
				 * Don't bother to trap on kernel writes,
				 * just record page as dirty.
				 */
				npte = PG_M;
				mem->clean = FALSE;
			} else
#ifdef ATTR
				if ((pmap_attributes[atop(pa - KERNBASE)] &
				    PMAP_ATTR_MOD) || !mem->clean)
#else
				if (!mem->clean)
#endif
					npte = PG_M;
			else
				npte = 0;
		}

#ifdef DEBUG
		enter_stats.managed++;
#endif
		/*
		 * Enter the pmap and virtual address into the
		 * physical to virtual map table.
		 */
		pv = pa_to_pvh(pa);
		s = splimp();
#ifdef DEBUG
		if (pmapdebug & PDB_ENTER)
			printf("pmap_enter: pv %x: was %x/%x/%x\n",
			       pv, pv->pv_va, pv->pv_pmap, pv->pv_next);
#endif
		if (pv->pv_pmap == NULL) {
			/*
			 * No entries yet, use header as the first entry
			 */
#ifdef DEBUG
			enter_stats.firstpv++;
#endif
			pv->pv_va = va;
			pv->pv_pmap = pmap;
			pv->pv_next = NULL;
		} else {
			/*
			 * There is at least one other VA mapping this page.
			 * Place this entry after the header.
			 *
			 * Note: the entry may already be in the table if
			 * we are only changing the protection bits.
			 */
			for (npv = pv; npv; npv = npv->pv_next)
				if (pmap == npv->pv_pmap && va == npv->pv_va) {
#ifdef DIAGNOSTIC
				    if (!pmap->pm_hash) {
					unsigned entry;

					entry = kvtopte(va)->pt_entry;
					if (!(entry & PG_V) ||
					    (entry & PG_FRAME) != pa)
			printf("found kva %x pa %x in pv_table but != %x\n",
				va, pa, entry);
				    } else {
					hp = &pmap->pm_hash[PMAP_HASH(va)];
					if ((hp->pmh_pte[0].high == (va |
					(pmap->pm_tlbpid <<
					VMMACH_TLB_PID_SHIFT)) &&
					(hp->pmh_pte[0].low & PG_FRAME) == pa) ||
					(hp->pmh_pte[1].high == (va |
					(pmap->pm_tlbpid <<
					VMMACH_TLB_PID_SHIFT)) &&
					(hp->pmh_pte[1].low & PG_FRAME) == pa))
						goto fnd;
			printf("found va %x pa %x in pv_table but !=\n",
				va, pa);
				    }
#endif
					goto fnd;
				}
			/* can this cause us to recurse forever? */
			npv = (pv_entry_t)
				malloc(sizeof *npv, M_VMPVENT, M_NOWAIT);
			npv->pv_va = va;
			npv->pv_pmap = pmap;
			npv->pv_next = pv->pv_next;
			pv->pv_next = npv;
#ifdef DEBUG
			if (!npv->pv_next)
				enter_stats.secondpv++;
#endif
		fnd:
			;
		}
		splx(s);
	} else {
		/*
		 * Assumption: if it is not part of our managed memory
		 * then it must be device memory which may be volitile.
		 */
#ifdef DEBUG
		enter_stats.unmanaged++;
#endif
		printf("pmap_enter: UNMANAGED ADDRESS va %x pa %x\n",
			va, pa); /* XXX */
		npte = (prot & VM_PROT_WRITE) ? PG_M : PG_RO;
	}

	/*
	 * The only time we need to flush the cache is if we
	 * execute from a physical address and then change the data.
	 * This is the best place to do this.
	 * pmap_protect() and pmap_remove() are mostly used to switch
	 * between R/W and R/O pages.
	 * NOTE: we only support cache flush for read only text.
	 */
	if (prot == (VM_PROT_READ | VM_PROT_EXECUTE))
		MachFlushICache(MACH_PHYS_TO_CACHED(pa), PAGE_SIZE);

	if (!pmap->pm_hash) {
		register pt_entry_t *pte;

		/* enter entries into kernel pmap */
		pte = kvtopte(va);
		npte |= pa | PG_V | PG_G;
		if (wired) {
			pmap->pm_stats.wired_count += pmaxpagesperpage;
			npte |= PG_WIRED;
		}
		i = pmaxpagesperpage;
		do {
			if (!(pte->pt_entry & PG_V)) {
				pmap->pm_stats.resident_count++;
				MachTLBWriteRandom(va, npte);
			} else {
#ifdef DIAGNOSTIC
				if (pte->pt_entry & PG_WIRED)
					panic("pmap_enter: kernel wired");
#endif
				/*
				 * Update the same virtual address entry.
				 */
				MachTLBUpdate(va, npte);
				printf("TLB update kva %x pte %x -> %x\n",
					va, pte->pt_entry, npte); /* XXX */
			}
			pte->pt_entry = npte;
			va += NBPG;
			npte += NBPG;
			pte++;
		} while (--i != 0);
		return;
	}

	/*
	 * Now validate mapping with desired protection/wiring.
	 * Assume uniform modified and referenced status for all
	 * PMAX pages in a MACH page.
	 */
	npte |= pa | PG_V;
	if (wired) {
		pmap->pm_stats.wired_count += pmaxpagesperpage;
		npte |= PG_WIRED;
	}
#ifdef DEBUG
	if (pmapdebug & PDB_ENTER)
		printf("pmap_enter: new pte value %x\n", npte);
#endif
	va |= (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
	i = pmaxpagesperpage;
	do {
		hp = &pmap->pm_hash[PMAP_HASH(va)];
		if (hp->pmh_pte[0].high == va &&
		    (hp->pmh_pte[0].low & PG_FRAME) == (npte & PG_FRAME))
			j = 0;
		else if (hp->pmh_pte[1].high == va &&
		    (hp->pmh_pte[1].low & PG_FRAME) == (npte & PG_FRAME))
			j = 1;
		else
			j = -1;
		if (j >= 0) {
#ifdef DEBUG
			enter_stats.cachehit++;
#endif
			if (!(hp->pmh_pte[j].low & PG_WIRED)) {
				/*
				 * Update the same entry.
				 */
				hp->pmh_pte[j].low = npte;
				MachTLBUpdate(va, npte);
			} else {
				/*
				 * Don't replace wired entries, just update
				 * the hardware TLB.
				 * Bug: routines to flush the TLB won't know
				 * that the entry is in the hardware.
				 */
				printf("pmap_enter: wired va %x %x\n", va,
					hp->pmh_pte[j].low); /* XXX */
				panic("pmap_enter: wired"); /* XXX */
				MachTLBWriteRandom(va, npte);
			}
			goto next;
		}
		if (!hp->pmh_pte[0].high)
			j = 0;
		else if (!hp->pmh_pte[1].high)
			j = 1;
		else
			j = -1;
		if (j >= 0) {
			pmap->pm_stats.resident_count++;
			hp->pmh_pte[j].high = va;
			hp->pmh_pte[j].low = npte;
			MachTLBWriteRandom(va, npte);
		} else {
#ifdef DEBUG
			enter_stats.cachehit++;
#endif
			if (!(hp->pmh_pte[1].low & PG_WIRED)) {
				MachTLBFlushAddr(hp->pmh_pte[1].high);
				pmap_remove_pv(pmap,
					hp->pmh_pte[1].high & PG_FRAME,
					hp->pmh_pte[1].low & PG_FRAME);
				hp->pmh_pte[1] = hp->pmh_pte[0];
				hp->pmh_pte[0].high = va;
				hp->pmh_pte[0].low = npte;
				MachTLBWriteRandom(va, npte);
			} else {
				/*
				 * Don't replace wired entries, just update
				 * the hardware TLB.
				 * Bug: routines to flush the TLB won't know
				 * that the entry is in the hardware.
				 */
				printf("pmap_enter: wired va %x %x\n", va,
					hp->pmh_pte[1].low); /* XXX */
				panic("pmap_enter: wired"); /* XXX */
				MachTLBWriteRandom(va, npte);
			}
		}
next:
		va += NBPG;
		npte += NBPG;
	} while (--i != 0);
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
	vm_offset_t va;
	boolean_t wired;
{
	register pmap_hash_t hp;
	u_int p;
	register int i, j;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_change_wiring(%x, %x, %x)\n", pmap, va, wired);
#endif
	if (pmap == NULL)
		return;

	p = wired ? PG_WIRED : 0;

	/*
	 * Don't need to flush the TLB since PG_WIRED is only in software.
	 */
	if (!pmap->pm_hash) {
		register pt_entry_t *pte;

		/* change entries in kernel pmap */
#ifdef DIAGNOSTIC
		if (va < VM_MIN_KERNEL_ADDRESS ||
		    va >= VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES*NPTEPG*NBPG)
			panic("pmap_change_wiring");
#endif
		pte = kvtopte(va);
		i = pmaxpagesperpage;
		if (!(pte->pt_entry & PG_WIRED) && p)
			pmap->pm_stats.wired_count += i;
		else if ((pte->pt_entry & PG_WIRED) && !p)
			pmap->pm_stats.wired_count -= i;
		do {
			if (!(pte->pt_entry & PG_V))
				continue;
			pte->pt_entry = (pte->pt_entry & ~PG_WIRED) | p;
			pte++;
		} while (--i != 0);
	} else if (pmap->pm_tlbpid >= 0 && pmap->pm_hash != zero_pmap_hash) {
		i = pmaxpagesperpage;
		va = (va & PG_FRAME) | (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
		do {
			hp = &pmap->pm_hash[PMAP_HASH(va)];
			if (hp->pmh_pte[0].high == va)
				j = 0;
			else if (hp->pmh_pte[1].high == va)
				j = 1;
			else {
				va += NBPG;
				continue;
			}
			if (!(hp->pmh_pte[j].low & PG_WIRED) && p)
				pmap->pm_stats.wired_count++;
			else if ((hp->pmh_pte[j].low & PG_WIRED) && !p)
				pmap->pm_stats.wired_count--;
			hp->pmh_pte[j].low = (hp->pmh_pte[j].low & ~PG_WIRED) | p;
			va += NBPG;
		} while (--i != 0);
	}
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
	register pmap_hash_t hp;
	register int i;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_extract(%x, %x) -> ", pmap, va);
#endif

	if (!pmap->pm_hash) {
#ifdef DIAGNOSTIC
		if (va < VM_MIN_KERNEL_ADDRESS ||
		    va >= VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES*NPTEPG*NBPG)
			panic("pmap_extract");
#endif
		pa = kvtopte(va)->pt_entry & PG_FRAME;
	} else if (pmap->pm_tlbpid >= 0) {
		hp = &pmap->pm_hash[PMAP_HASH(va)];
		va = (va & PG_FRAME) | (pmap->pm_tlbpid << VMMACH_TLB_PID_SHIFT);
		if (hp->pmh_pte[0].high == va)
			pa = hp->pmh_pte[0].low & PG_FRAME;
		else if (hp->pmh_pte[1].high == va)
			pa = hp->pmh_pte[1].low & PG_FRAME;
		else
			pa = 0;
	} else
		pa = 0;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("%x\n", pa);
#endif
	return (pa);
}

/*
 *	Copy the range specified by src_addr/len
 *	from the source map to the range dst_addr/len
 *	in the destination map.
 *
 *	This routine is only advisory and need not do anything.
 */
void
pmap_copy(dst_pmap, src_pmap, dst_addr, len, src_addr)
	pmap_t dst_pmap;
	pmap_t src_pmap;
	vm_offset_t dst_addr;
	vm_size_t len;
	vm_offset_t src_addr;
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
void
pmap_update()
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_update()\n");
#endif
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
	pmap_t pmap;
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_collect(%x)\n", pmap);
#endif
}

/*
 *	pmap_zero_page zeros the specified (machine independent)
 *	page.
 */
void
pmap_zero_page(phys)
	vm_offset_t phys;
{
	register int *p, *end;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_zero_page(%x)\n", phys);
#endif
	p = (int *)MACH_PHYS_TO_CACHED(phys);
	end = p + PAGE_SIZE / sizeof(int);
	do {
		p[0] = 0;
		p[1] = 0;
		p[2] = 0;
		p[3] = 0;
		p += 4;
	} while (p != end);
}

/*
 *	pmap_copy_page copies the specified (machine independent)
 *	page.
 */
void
pmap_copy_page(src, dst)
	vm_offset_t src, dst;
{
	register int *s, *d, *end;
	register int tmp0, tmp1, tmp2, tmp3;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_copy_page(%x, %x)\n", src, dst);
#endif
	s = (int *)MACH_PHYS_TO_CACHED(src);
	d = (int *)MACH_PHYS_TO_CACHED(dst);
	end = s + PAGE_SIZE / sizeof(int);
	do {
		tmp0 = s[0];
		tmp1 = s[1];
		tmp2 = s[2];
		tmp3 = s[3];
		d[0] = tmp0;
		d[1] = tmp1;
		d[2] = tmp2;
		d[3] = tmp3;
		s += 4;
		d += 4;
	} while (s != end);
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
}

/*
 *	Clear the modify bits on the specified physical page.
 */
void
pmap_clear_modify(pa)
	vm_offset_t pa;
{
	pmap_hash_t hp;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_clear_modify(%x)\n", pa);
#endif
#ifdef ATTR
	pmap_attributes[atop(pa - KERNBASE)] &= ~PMAP_ATTR_MOD;
#endif
}

/*
 *	pmap_clear_reference:
 *
 *	Clear the reference bit on the specified physical page.
 */
void
pmap_clear_reference(pa)
	vm_offset_t pa;
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_clear_reference(%x)\n", pa);
#endif
#ifdef ATTR
	pmap_attributes[atop(pa - KERNBASE)] &= ~PMAP_ATTR_REF;
#endif
}

/*
 *	pmap_is_referenced:
 *
 *	Return whether or not the specified physical page is referenced
 *	by any physical maps.
 */
boolean_t
pmap_is_referenced(pa)
	vm_offset_t pa;
{
#ifdef ATTR
	return (pmap_attributes[atop(pa - KERNBASE)] & PMAP_ATTR_REF);
#else
	return (FALSE);
#endif
}

/*
 *	pmap_is_modified:
 *
 *	Return whether or not the specified physical page is modified
 *	by any physical maps.
 */
boolean_t
pmap_is_modified(pa)
	vm_offset_t pa;
{
#ifdef ATTR
	return (pmap_attributes[atop(pa - KERNBASE)] & PMAP_ATTR_MOD);
#else
	return (FALSE);
#endif
}

vm_offset_t
pmap_phys_address(ppn)
	int ppn;
{

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_phys_address(%x)\n", ppn);
#endif
	panic("pmap_phys_address"); /* XXX */
	return (pmax_ptob(ppn));
}

/*
 * Miscellaneous support routines
 */

/*
 * Allocate a hardware PID and return it.
 * Also, change the hardwired TLB entry to point to the current TLB cache.
 * This is called by swtch().
 */
int
pmap_alloc_tlbpid(p)
	register struct proc *p;
{
	register pmap_t pmap;
	register u_int i;
	register int id;

	pmap = &p->p_vmspace->vm_pmap;
	if ((id = pmap->pm_tlbpid) >= 0) {
		if (pmap->pm_flags & PM_MODIFIED) {
			pmap->pm_flags &= ~PM_MODIFIED;
			MachTLBFlushPID(id);
		}
		goto done;
	}

	if ((i = whichpids[0]) != 0xFFFFFFFF)
		id = 0;
	else if ((i = whichpids[1]) != 0xFFFFFFFF)
		id = 32;
	else {
		register struct proc *q;
		register pmap_t q_pmap;

		/*
		 * Have to find a tlbpid to recycle.
		 * There is probably a better way to do this.
		 */
		for (q = (struct proc *)allproc; q != NULL; q = q->p_nxt) {
			q_pmap = &q->p_vmspace->vm_pmap;
			if ((id = q_pmap->pm_tlbpid) < 0)
				continue;
			if (q->p_stat != SRUN)
				goto fnd;
		}
		if (id < 0)
			panic("TLBPidAlloc");
	fnd:
		printf("pmap_alloc_tlbpid: recycle pid %d (%s) tlbpid %d\n",
			q->p_pid, q->p_comm, id); /* XXX */
		/*
		 * Even though the virtual to physical mapping hasn't changed,
		 * we need to clear the PID tag in the high entry of the cache.
		 */
		if (q_pmap->pm_hash != zero_pmap_hash) {
			register pmap_hash_t hp;
			register int j;

			hp = q_pmap->pm_hash;
			for (i = 0; i < PMAP_HASH_NUM_ENTRIES; i++, hp++) {
			    for (j = 0; j < 2; j++) {
				if (!hp->pmh_pte[j].high)
					continue;

				if (hp->pmh_pte[j].low & PG_WIRED) {
					printf("Clearing wired user entry! h %x l %x\n", hp->pmh_pte[j].high, hp->pmh_pte[j].low);
					panic("pmap_alloc_tlbpid: wired");
				}
				pmap_remove_pv(q_pmap,
					hp->pmh_pte[j].high & PG_FRAME,
					hp->pmh_pte[j].low & PG_FRAME);
				hp->pmh_pte[j].high = 0;
				q_pmap->pm_stats.resident_count--;
			    }
			}
		}
		q_pmap->pm_tlbpid = -1;
		MachTLBFlushPID(id);
#ifdef DEBUG
		remove_stats.pidflushes++;
#endif
		pmap->pm_tlbpid = id;
		goto done;
	}
	while (i & 1) {
		i >>= 1;
		id++;
	}
	whichpids[id >> 5] |= 1 << (id & 0x1F);
	pmap->pm_tlbpid = id;
done:
	/*
	 * Map in new TLB cache.
	 */
	if (pmap == cur_pmap)
		return (id);
	cur_pmap = pmap;
	for (i = 0; i < PMAP_HASH_UPAGES; i++) {
		MachTLBWriteIndexed(i + UPAGES,
			(PMAP_HASH_UADDR + (i << PGSHIFT)) |
				(id << VMMACH_TLB_PID_SHIFT),
			pmap->pm_hash_ptes[i]);
	}
	return (id);
}

/*
 * Remove a physical to virtual address translation.
 */
void
pmap_remove_pv(pmap, va, pa)
	pmap_t pmap;
	vm_offset_t va, pa;
{
	register pv_entry_t pv, npv;
	int s;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_remove_pv(%x, %x, %x)\n", pmap, va, pa);
#endif
	/*
	 * Remove page from the PV table (raise IPL since we
	 * may be called at interrupt time).
	 */
	if (!IS_VM_PHYSADDR(pa))
		return;
	pv = pa_to_pvh(pa);
	s = splimp();
	/*
	 * If it is the first entry on the list, it is actually
	 * in the header and we must copy the following entry up
	 * to the header.  Otherwise we must search the list for
	 * the entry.  In either case we free the now unused entry.
	 */
	if (pmap == pv->pv_pmap && va == pv->pv_va) {
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
		for (npv = pv->pv_next; npv; pv = npv, npv = npv->pv_next) {
#ifdef DEBUG
			remove_stats.pvsearch++;
#endif
			if (pmap == npv->pv_pmap && va == npv->pv_va)
				goto fnd;
		}
#ifdef DIAGNOSTIC
		printf("pmap_remove_pv(%x, %x, %x) not found\n", pmap, va, pa);
		panic("pmap_remove_pv");
#endif
	fnd:
		pv->pv_next = npv->pv_next;
		free((caddr_t)npv, M_VMPVENT);
	}
	splx(s);
}

#ifdef DEBUG
pmap_print(pmap)
	pmap_t pmap;
{
	register pmap_hash_t hp;
	register int i, j;

	printf("\tpmap_print(%x)\n", pmap);

	if (pmap->pm_hash == zero_pmap_hash) {
		printf("pm_hash == zero\n");
		return;
	}
	if (pmap->pm_hash == (pmap_hash_t)0) {
		printf("pm_hash == kernel\n");
		return;
	}
	hp = pmap->pm_hash;
	for (i = 0; i < PMAP_HASH_NUM_ENTRIES; i++, hp++) {
	    for (j = 0; j < 2; j++) {
		if (!hp->pmh_pte[j].high)
			continue;
		printf("%d: hi %x low %x\n", i, hp->pmh_pte[j].high, hp->pmh_pte[j].low);
	    }
	}
}
#endif
