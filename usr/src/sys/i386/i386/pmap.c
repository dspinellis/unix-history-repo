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
 * Derived from hp300 version by Mike Hibler, this version by William
 * Jolitz uses a recursive map [a pde points to the page directory] to
 * map the page tables using the pagetables themselves. This is done to
 * reduce the impact on kernel virtual memory for lots of sparse address
 * space, and to reduce the cost of memory to each process.
 *
 *	Derived from: hp300/@(#)pmap.c	7.1 (Berkeley) 12/5/90
 *	@(#)pmap.c	7.1	%G%
 */

/*
 *	Reno i386 version, from Mike Hibler's hp300 version.
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
#include "../vm/vm_param.h"
#include "user.h"
#include "proc.h"
#include "lock.h"
#include "malloc.h"

#include "../vm/pmap.h"
#include "../vm/vm_map.h"
#include "../vm/vm_kern.h"
#include "../vm/vm_prot.h"
#include "../vm/vm_page.h"
#include "../vm/vm_pageout.h"

#include "machine/isa.h"

/*
 * Allocate various and sundry SYSMAPs used in the days of old VM
 * and not yet converted.  XXX.
 */
#define BSDVM_COMPAT	1

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

int debugmap = 0;
int pmapdebug = 0 /*0xffff*/;
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
#define PDB_PDRTAB	0x0400
#define PDB_PARANOIA	0x2000
#define PDB_WIRING	0x4000
#define PDB_PVDUMP	0x8000

int pmapvacflush = 0;
#define	PVF_ENTER	0x01
#define	PVF_REMOVE	0x02
#define	PVF_PROTECT	0x04
#define	PVF_TOTAL	0x80
#endif

/*
 * Get PDEs and PTEs for user/kernel address space
 */
#define	pmap_pde(m, v)	(&((m)->pm_pdir[((vm_offset_t)(v) >> PD_SHIFT)&1023]))
/*#define pmap_pte(m, v)	(&((m)->pm_ptab[(vm_offset_t)(v) >> PG_SHIFT]))*/
/*#define pmap_pte(m, v)	(vtopte(v))*/

#define pmap_pte_pa(pte)	(*(int *)(pte) & PG_FRAME)

#define pmap_pde_v(pte)		((pte)->pd_v)
#define pmap_pte_w(pte)		((pte)->pg_w)
/* #define pmap_pte_ci(pte)	((pte)->pg_ci) */
#define pmap_pte_m(pte)		((pte)->pg_m)
#define pmap_pte_u(pte)		((pte)->pg_u)
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

#ifdef junk
/*
 * Kernel page directory and page table and page table map.
 * The page table map gives us a level of indirection we need to dynamically
 * expand the page table.  It is essentially a copy of the page directory table
 * with PTEs instead of PDEs.  All are initialized in locore at boot time.
 * Sysmap will initially contain VM_KERNEL_PT_PAGES pages of PTEs.
 *
 * PTDtabzero is an prototype page directory table which all processes
 * share til they reference something.
 */
pd_entry_t	*SysPDT;
pt_entry_t	*Sysmap, *Sysptmap;
pd_entry_t	*PDTtabzero;
#if BSDVM_COMPAT
vm_size_t	Sysptsize = VM_KERNEL_PT_PAGES + 4 / NPTEPG;
#else
vm_size_t	Sysptsize = VM_KERNEL_PT_PAGES;
#endif
#endif

struct pmap	kernel_pmap_store;
pmap_t		kernel_pmap;
vm_map_t	pt_map;

vm_offset_t    	avail_start;	/* PA of first available physical page */
vm_offset_t	avail_end;	/* PA of last available physical page */
vm_size_t	mem_size;	/* memory size in bytes */
vm_offset_t	virtual_avail;  /* VA of first avail page (after kernel bss)*/
vm_offset_t	virtual_end;	/* VA of last avail page (end of kernel AS) */
vm_offset_t	vm_first_phys;	/* PA of first managed page */
vm_offset_t	vm_last_phys;	/* PA just past last managed page */
int		i386pagesperpage;	/* PAGE_SIZE / I386_PAGE_SIZE */
boolean_t	pmap_initialized = FALSE;	/* Has pmap_init completed? */
char		*pmap_attributes;	/* reference and modify bits */

boolean_t	pmap_testbit();
void		pmap_enter_ptpage(), pmap_clear_modify();

#if BSDVM_COMPAT
#include "msgbuf.h"

/*
 * All those kernel PT submaps that BSD is so fond of
 */
struct pte	*Swtchmap, *CMAP1, *CMAP2, *mmap;
caddr_t		Swtchbase, CADDR1, CADDR2, vmmap;
struct pte	*msgbufmap;
struct msgbuf	*msgbufp;
#endif

/*
 *	Bootstrap the system enough to run with virtual memory.
 *	Map the kernel's code and data, and allocate the system page table.
 *
 *	On the I386 this is called after mapping has already been enabled
 *	and just syncs the pmap module with what has already been done.
 *	[We can't call it easily with mapping off since the kernel is not
 *	mapped with PA == VA, hence we would have to relocate every address
 *	from the linked base (virtual) address 0xFE000000 to the actual
 *	(physical) address starting relative to 0]
 */
struct pte *pmap_pte();

extern vm_offset_t	atdevbase;
int
pmap_bootstrap(firstaddr, loadaddr)
	vm_offset_t firstaddr;
	vm_offset_t loadaddr;
{
#if BSDVM_COMPAT
	vm_offset_t va;
	struct pte *pte;
#endif
	extern vm_offset_t maxmem, physmem;
extern int IdlePTD;

	avail_start = 0x100000 /* firstaddr */; /* XXX */
	avail_end = maxmem << PG_SHIFT;

	/* XXX: allow for msgbuf */
	avail_end -= i386_round_page(sizeof(struct msgbuf));

	mem_size = physmem << PG_SHIFT;
	virtual_avail = atdevbase + 0x100000 - 0xa0000 + 10*NBPG;
	virtual_end = VM_MAX_KERNEL_ADDRESS;
printf("avail [%x %x] virtual [%x %x]\n",
	avail_start, avail_end, virtual_avail, virtual_end);
printf("cr3 %x", rcr3());
	i386pagesperpage = PAGE_SIZE / I386_PAGE_SIZE;

	/*
	 * Initialize protection array.
	 */
	i386_protection_init();

	/*
	 * The kernel's pmap is statically allocated so we don't
	 * have to use pmap_create, which is unlikely to work
	 * correctly at this part of the boot sequence.
	 */
	kernel_pmap = &kernel_pmap_store;

#ifdef notdef
	/*
	 * Create Kernel page directory table and page maps.
	 */
	bzero(firstaddr, 4*NBPG);
	kernel_pmap->pm_pdir = firstaddr + VM_MIN_KERNEL_ADDRESS;
	kernel_pmap->pm_ptab = firstaddr + VM_MIN_KERNEL_ADDRESS + NBPG;

	firstaddr += NBPG;
	for (x = i386_btod(VM_MIN_KERNEL_ADDRESS);
		x < i386_btod(VM_MIN_KERNEL_ADDRESS)+3; x++) {
			struct pde *pde;
		pde = kernel_pmap->pm_pdir + x;
		*(int *)pde = firstaddr + x*NBPG | PG_V | PG_KW;
	}
#else
kernel_pmap->pm_pdir = (pd_entry_t *)(0xfe000000 + IdlePTD);
#endif


	simple_lock_init(&kernel_pmap->pm_lock);
	kernel_pmap->pm_count = 1;

#if BSDVM_COMPAT
	/*
	 * Allocate all the submaps we need
	 */
#define	SYSMAP(c, p, v, n)	\
	v = (c)va; va += ((n)*I386_PAGE_SIZE); p = pte; pte += (n);

	va = virtual_avail;
	pte = pmap_pte(kernel_pmap, va);

printf("pte %x va %x\n", pte, va);
	SYSMAP(caddr_t		,Swtchmap	,Swtchbase   ,1		)
	SYSMAP(caddr_t		,CMAP1		,CADDR1	   ,1		)
	SYSMAP(caddr_t		,CMAP2		,CADDR2	   ,1		)
	SYSMAP(caddr_t		,mmap		,vmmap	   ,1		)
	SYSMAP(struct msgbuf *	,msgbufmap	,msgbufp   ,1		)
printf("pte %x va %x\n", pte, va);
	virtual_avail = va;
#endif

*(int *)PTD = 0;
load_cr3(rcr3());

	return (firstaddr);
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
	int		rv;
extern int KPTphys;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_init(%x, %x)\n", phys_start, phys_end);
#endif
	/*
	 * Now that kernel map has been allocated, we can mark as
	 * unavailable regions which we have mapped in locore.
	 */
printf("atdevbase %x", atdevbase);
	addr = atdevbase;
	(void) vm_map_find(kernel_map, VM_OBJECT_NULL, (vm_offset_t) 0,
			   &addr, (0x100000-0xa0000), FALSE);
printf("addr %x", addr);
#ifdef notdef
	if (addr != ATDevmem)
		goto bogons;
#endif
	addr = (vm_offset_t) 0xfe000000+KPTphys/* *NBPG */;
	vm_object_reference(kernel_object);
	(void) vm_map_find(kernel_map, kernel_object, addr,
			   &addr, 2*NBPG, FALSE);
	/*
	 * If this fails it is probably because the static portion of
	 * the kernel page table isn't big enough and we overran the
	 * page table map.   Need to adjust pmap_size() in i386_init.c.
	 */
#ifdef notdef
	if (addr != (vm_offset_t)Sysmap)
		goto bogons;
#endif

#ifdef not
	/* u. is actually in process address space */
	addr = (vm_offset_t) &u;
	vm_object_reference(kernel_object);
	(void) vm_map_find(kernel_map, kernel_object, addr,
			   &addr, i386_ptob(UPAGES), FALSE);
	if (addr != (vm_offset_t)&u)
bogons:
		panic("pmap_init: bogons in the VM system!\n");
#endif

#ifdef DEBUGx
	if (pmapdebug & PDB_INIT) {
		printf("pmap_init: SysPDT %x, Sysmap %x, Sysptmap %x\n",
		       SysPDT, Sysmap, Sysptmap);
		printf("  pstart %x, pend %x, vstart %x, vend %x\n",
		       avail_start, avail_end, virtual_avail, virtual_end);
	}
#endif

	/*
	 * Allocate memory for random pmap data structures.  Includes the
	 * pv_head_table and pmap_attributes.
	 */
	npg = atop(phys_end - phys_start);
	s = (vm_size_t) (sizeof(struct pv_entry) * npg + npg);
	s = round_page(s);
	addr = (vm_offset_t) kmem_alloc(kernel_map, s);
	pv_table = (pv_entry_t) addr;
	addr += sizeof(struct pv_entry) * npg;
	pmap_attributes = (char *) addr;
#ifdef DEBUG
	if (pmapdebug & PDB_INIT)
		printf("pmap_init: %x bytes (%x pgs): tbl %x attr %x\n",
		       s, npg, pv_table, pmap_attributes);
#endif

#ifdef notdef
	/* we don't know if we will use kpt pages at all yet */
	/*
	 * Allocate physical memory for kernel PT pages and their management.
	 * We need 1 PT page per possible task plus some slop.
	 */
	npg = min(atop(I386_MAX_KPTSIZE), nproc+16);
	s = ptoa(npg) + round_page(npg * sizeof(struct kpt_page));

	/*
	 * Verify that space will be allocated in region for which
	 * we already have kernel PT pages.
	 */
	addr = 0;
	rv = vm_map_find(kernel_map, VM_OBJECT_NULL, 0, &addr, s, TRUE);
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
		addr2 -= I386_PAGE_SIZE;
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
#endif

#ifdef notdef
	/* we don't know yet if we want a upt map elsewhere */
	/*
	 * Slightly modified version of kmem_suballoc() to get page table
	 * map where we want it.
	 */
	addr = I386_PTBASE;
	s = min(I386_PTMAXSIZE, nproc*I386_MAX_PTSIZE);
	addr2 = addr + s;
	rv = vm_map_find(kernel_map, VM_OBJECT_NULL, 0, &addr, s, TRUE);
	if (rv != KERN_SUCCESS)
		panic("pmap_init: cannot allocate space for PT map");
	pmap_reference(vm_map_pmap(kernel_map));
	pt_map = vm_map_create(vm_map_pmap(kernel_map), addr, addr2, TRUE);
	if (pt_map == VM_MAP_NULL)
		panic("pmap_init: cannot create pt_map");
	rv = vm_map_submap(kernel_map, addr, addr2, pt_map);
	if (rv != KERN_SUCCESS)
		panic("pmap_init: cannot map range to pt_map");
#ifdef DEBUG
	if (pmapdebug & PDB_INIT)
		printf("pmap_init: pt_map [%x - %x)\n", addr, addr2);
#endif
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
 *
 * [ just allocate a ptd and mark it uninitialize -- should we track
 *   with a table which process has which ptd? -wfj]
 */
pmap_t
pmap_create(size)
	vm_size_t	size;
{
	register pmap_t pmap;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_CREATE))
		pg("pmap_create(%x)\n", size);
#endif
	/*
	 * Software use map does not need a pmap
	 */
	if (size)
		return(PMAP_NULL);

	/* XXX: is it ok to wait here? */
	pmap = (pmap_t) malloc(sizeof *pmap, M_VMPMAP, M_WAITOK);
	if (pmap == PMAP_NULL)
		panic("pmap_create: cannot allocate a pmap");

	/*
	 * No need to allocate page table space yet but we do need a
	 * valid page directory table.
	 */
#ifdef junk
	pmap->pm_ptab = PT_ENTRY_NULL;
#endif
	pmap->pm_pdir = (pd_entry_t *) kmem_alloc(kernel_map, NBPG);
printf("pdir %x pdt.. %x ", pmap->pm_pdir, *(int*)(PTD+KPTDI_FIRST));
bcopy(PTD+KPTDI_FIRST, pmap->pm_pdir+KPTDI_FIRST, (KPTDI_LAST-KPTDI_FIRST+1)*4);
*(int *)(pmap->pm_pdir+PTDPTDI) =
	(int)pmap_extract(kernel_pmap, pmap->pm_pdir) | PG_V | PG_URKW;
	/*pmap->pm_ptobj = (caddr_t) vm_object_allocate(1024);*/
	pmap->pm_dref = 0;
	pmap->pm_count = 1;
	simple_lock_init(&pmap->pm_lock);
	pmap->pm_stats.resident_count = 0;
	pmap->pm_stats.wired_count = 0;
	pmap->pm_ptpages = 0;
	return(pmap);
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
		pg("pmap_destroy(%x)\n", pmap);
#endif
	if (pmap == PMAP_NULL)
		return;

	simple_lock(&pmap->pm_lock);
	count = --pmap->pm_count;
	simple_unlock(&pmap->pm_lock);
	if (count)
		return;

#ifdef junk
	if (pmap->pm_ptab)
		kmem_free_wakeup(pt_map, (vm_offset_t)pmap->pm_ptab,
				 I386_MAX_PTSIZE);
#endif
	/*vm_object_deallocate((vm_object_t)pmap->pm_ptobj);*/
	kmem_free(kernel_map, (vm_offset_t)pmap->pm_pdir, 1);
	free((caddr_t)pmap, M_VMPMAP);
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
	if (pmap != PMAP_NULL) {
		simple_lock(&pmap->pm_lock);
		pmap->pm_count++;
		simple_unlock(&pmap->pm_lock);
	}
}

#define DCIS()
#define DCIU()
#define DCIA()
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
	register int ix;
	pmap_t ptpmap;
	int *pde, s, bits;
	boolean_t firstpage = TRUE;
	boolean_t flushcache = FALSE;
#ifdef DEBUG
	pt_entry_t opte;

	if (pmapdebug & (PDB_FOLLOW|PDB_REMOVE|PDB_PROTECT))
		pg("pmap_remove(%x, %x, %x)\n", pmap, sva, eva);
#endif

	if (pmap == PMAP_NULL)
		return;

#ifdef DEBUG
	remove_stats.calls++;
#endif
	for (va = sva; va < eva; va += PAGE_SIZE) {
		/*
		 * Weed out invalid mappings.
		 * Note: we assume that the page directory table is always allocated.
		 */
		if (!pmap_pde_v(pmap_pde(pmap, va))) {
			va = i386_round_pdr(va + PAGE_SIZE) - PAGE_SIZE;
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
			printf("remove: invalidating %x ptes at %x\n",
			       i386pagesperpage, pte);
#endif
		bits = ix = 0;
		do {
			bits |= *(int *)pte & (PG_U|PG_M);
			*(int *)pte++ = 0;
			/*TBIS(va + ix * I386_PAGE_SIZE);*/
		} while (++ix != i386pagesperpage);
		/*if (pmap == u.u_procp->p_map->pmap)
			pmap_activate(pmap, (struct pcb *)u.u_procp->p_addr); */
		load_cr3(u.u_pcb.pcb_ptd);

#ifdef unneeded
		/*
		 * For user mappings decrement the wiring count on
		 * the PT page.
		 */
		if (pmap != kernel_pmap) {
			vm_page_t	mem;

			mem = vm_page_lookup(pmap->pm_ptobj,
				(vm_offset_t)pdei(va));
			mem->wire_count--;
		}
#endif

		/*
		 * Remove from the PV table (raise IPL since we
		 * may be called at interrupt time).
		 */
		if (pa < vm_first_phys || pa >= vm_last_phys)
			continue;
		pv = pa_to_pvh(pa);
		pde = (int *)0;
		s = splimp();
		/*
		 * If it is the first entry on the list, it is actually
		 * in the header and we must copy the following entry up
		 * to the header.  Otherwise we must search the list for
		 * the entry.  In either case we free the now unused entry.
		 */
		if (pmap == pv->pv_pmap && va == pv->pv_va) {
			pde = (int *)pv->pv_ptpde;
			ptpmap = pv->pv_ptpmap;
			npv = pv->pv_next;
			if (npv) {
				*pv = *npv;
				free((caddr_t)npv, M_VMPVENT);
			} else
				pv->pv_pmap = PMAP_NULL;
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
			if (npv == PV_ENTRY_NULL)
				panic("pmap_remove: PA not in pv_tab");
#endif
			pde = (int *)npv->pv_ptpde;
			ptpmap = npv->pv_ptpmap;
			pv->pv_next = npv->pv_next;
			free((caddr_t)npv, M_VMPVENT);
			pv = pa_to_pvh(pa);
		}
		/*
		 * If only one mapping left we no longer need to cache inhibit
		 */
		if (pv->pv_pmap &&
		    pv->pv_next == PV_ENTRY_NULL && (pv->pv_flags & PV_CI)) {
#ifdef DEBUG
			if (pmapdebug & PDB_CACHE)
				pg("remove: clearing CI for pa %x\n", pa);
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
		 * mapping from the associated page directory table.
		 */
		if (pde) {
#ifdef DEBUG
			remove_stats.ptinvalid++;
			if (pmapdebug & (PDB_REMOVE|PDB_PTPAGE)) {
				pg("remove: pde was %x@%x pte was %x@%x\n",
				       *pde, pde,
				       *(int *)&opte, pmap_pte(pmap, va));
			}
#endif
			*pde = 0;
			/*{ vm_page_t	mem;
					mem = vm_page_lookup(pmap->pm_ptobj,
				(vm_offset_t)pdei(va));
					mem->wire_count = 0
					vm_page_free(mem);*/
			/*
			 * If it was a user PT page, we decrement the
			 * reference count on the page directory table as well,
			 * freeing it if it is now empty.
			 */
			if (ptpmap != kernel_pmap) {
#ifdef DEBUG
				if (pmapdebug & (PDB_REMOVE|PDB_PDRTAB))
					pg("remove: pdir %x, refcnt %d\n",
					       ptpmap->pm_pdir,
					       ptpmap->pm_dref - 1);
				/*if ((pmapdebug & PDB_PARANOIA) &&
				    ptpmap->pm_pdir != (struct pd_entry_t *)trunc_page(pde))
					panic("remove: bogus pde");*/
#endif
			}
			/*if (ptpmap == kernel_pmap)
				TBIAS();
			else
				TBIAU(); */
			pv->pv_flags &= ~PV_PTPAGE;
			ptpmap->pm_ptpages--;
		}
		/*
		 * Update saved attributes for managed page
		 */
		pmap_attributes[pa_index(pa)] |= bits;
		splx(s);
	}
#ifdef DEBUGx
	if (pmapvacflush & PVF_REMOVE) {
		if (pmapvacflush & PVF_TOTAL)
			DCIA();
		else if (pmap == kernel_pmap)
			DCIS();
		else
			DCIU();
	}
#endif
	/*if (flushcache) {
		if (pmap == kernel_pmap) {
			DCIS();
#ifdef DEBUG
			remove_stats.sflushes++;
#endif
		} else {
			DCIU();
#ifdef DEBUG
			remove_stats.uflushes++;
#endif
		}
	}*/
}

/*
 *	Routine:	pmap_remove_all
 *	Function:
 *		Removes this physical page from
 *		all physical maps in which it resides.
 *		Reflects back modify bits to the pager.
 */
void
pmap_remove_all(pa)
	vm_offset_t pa;
{
	register pv_entry_t pv;
	int s;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_REMOVE|PDB_PROTECT))
		pg("pmap_remove_all(%x)\n", pa);
#endif
	/*
	 * Not one of ours
	 */
	if (pa < vm_first_phys || pa >= vm_last_phys)
{
pg("RET! ");
		return;
}

	pv = pa_to_pvh(pa);
	s = splimp();
	/*
	 * Do it the easy way for now
	 */
	while (pv->pv_pmap != PMAP_NULL) {
#ifdef DEBUG
		if (!pmap_pde_v(pmap_pde(pv->pv_pmap, pv->pv_va)) ||
		    pmap_pte_pa(pmap_pte(pv->pv_pmap, pv->pv_va)) != pa)
			panic("pmap_remove_all: bad mapping");
#endif
		pmap_remove(pv->pv_pmap, pv->pv_va, pv->pv_va + PAGE_SIZE);
	}
	splx(s);
}

/*
 *	Routine:	pmap_copy_on_write
 *	Function:
 *		Remove write privileges from all
 *		physical maps for this physical page.
 */
void
pmap_copy_on_write(pa)
	vm_offset_t pa;
{
#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_PROTECT))
		pg("pmap_copy_on_write(%x)\n", pa);
#endif
	pmap_changebit(pa, PG_RO, TRUE);
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
	register int ix;
	int i386prot;
	boolean_t firstpage = TRUE;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_PROTECT))
		pg("pmap_protect(%x, %x, %x, %x)\n", pmap, sva, eva, prot);
#endif
	if (pmap == PMAP_NULL)
		return;

/*
	if ((prot & VM_PROT_READ) == VM_PROT_NONE) {
		pmap_remove(pmap, sva, eva);
		return;
	}
	if (prot & VM_PROT_WRITE)
		return;*/

	pte = pmap_pte(pmap, sva);
	if(!pte) return;
	for (va = sva; va < eva; va += PAGE_SIZE) {
		/*
		 * Page table page is not allocated.
		 * Skip it, we don't want to force allocation
		 * of unnecessary PTE pages just to set the protection.
		 */
		if (!pmap_pde_v(pmap_pde(pmap, va))) {
			/* XXX: avoid address wrap around */
			if (va >= i386_trunc_pdr((vm_offset_t)-1))
				break;
			va = i386_round_pdr(va + PAGE_SIZE) - PAGE_SIZE;
			pte = pmap_pte(pmap, va);
			pte += i386pagesperpage;
			continue;
		}
		if(!pte) return;
		/*
		 * Page not valid.  Again, skip it.
		 * Should we do this?  Or set protection anyway?
		 */
		if (!pmap_pte_v(pte)) {
			pte += i386pagesperpage;
			continue;
		}
		ix = 0;
		i386prot = pte_prot(pmap, prot);
		if(va < UPT_MAX_ADDRESS)
			i386prot |= 2 /*PG_u*/;
		do {
			/* clear VAC here if PG_RO? */
			pmap_pte_set_prot(pte++, i386prot);
			/*TBIS(va + ix * I386_PAGE_SIZE);*/
		} while (++ix != i386pagesperpage);
	}
	if (pmap == u.u_procp->p_map->pmap)
		pmap_activate(pmap, (struct pcb *)u.u_procp->p_addr);
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
	register int npte, ix;
	vm_offset_t opa;
	boolean_t cacheable = TRUE;
	boolean_t checkpv = TRUE;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_ENTER))
		printf("pmap_enter(%x, %x, %x, %x, %x)\n",
		       pmap, va, pa, prot, wired);
#endif
	if (pmap == PMAP_NULL)
		return;

	if(va > VM_MAX_KERNEL_ADDRESS)panic("pmap_enter: toobig");
	/* also, should not muck with PTD va! */

#ifdef DEBUG
	if (pmap == kernel_pmap)
		enter_stats.kernel++;
	else
		enter_stats.user++;
#endif
#ifdef junk
[should insure we have 1)pagetable for u./kstack & page ]
	/*
	 * For user mapping, allocate kernel VM resources if necessary.
	 */
	if (pmap->pm_ptab == PT_ENTRY_NULL)
		pmap->pm_ptab = (pt_entry_t *)
			kmem_alloc_wait(pt_map, I386_MAX_PTSIZE);
#endif

	/*
	 * Page Directory table entry not valid, we need a new PT page
	 */
	if (!pmap_pde_v(pmap_pde(pmap, va)))
{
pg("ptdi %x", pmap->pm_pdir[PTDPTDI]);
		pmap_enter_ptpage(pmap, va);
}

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
				pg("enter: wiring change -> %x\n", wired);
#endif
			if (wired)
				pmap->pm_stats.wired_count++;
			else
				pmap->pm_stats.wired_count--;
#ifdef DEBUG
			enter_stats.wchange++;
#endif
		}
#ifdef junk
		/*
		 * Retain cache inhibition status
		 */
		checkpv = FALSE;
		if (pmap_pte_ci(pte))
			cacheable = FALSE;
#endif
		goto validate;
	}

	/*
	 * Mapping has changed, invalidate old range and fall through to
	 * handle validating new mapping.
	 */
	if (opa) {
#ifdef DEBUG
		if (pmapdebug & PDB_ENTER)
			pg("enter: removing old mapping %x pa %x\n", va, opa);
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
	if (pmap != kernel_pmap) {
		vm_page_t	mem;

	/*	mem = vm_page_lookup(pmap->pm_ptobj, (vm_offset_t)pdei(va));*/
		mem->wire_count++;
	}

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
		if (pv->pv_pmap == PMAP_NULL) {
#ifdef DEBUG
			enter_stats.firstpv++;
#endif
			pv->pv_va = va;
			pv->pv_pmap = pmap;
			pv->pv_next = PV_ENTRY_NULL;
			pv->pv_ptpde = PD_ENTRY_NULL;
			pv->pv_ptpmap = PMAP_NULL;
			pv->pv_flags = 0;
		}
		/*
		 * There is at least one other VA mapping this page.
		 * Place this entry after the header.
		 */
		else {
/*printf("second time: ");*/
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
			npv->pv_ptpde = PD_ENTRY_NULL;
			npv->pv_ptpmap = PMAP_NULL;
			pv->pv_next = npv;
#ifdef DEBUG
			if (!npv->pv_next)
				enter_stats.secondpv++;
#endif
		splx(s);
		}
	}
	/*
	 * Assumption: if it is not part of our managed memory
	 * then it must be device memory which may be volitile.
	 */
	if (pmap_initialized) {
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
	 * I386 pages in a MACH page.
	 */
	npte = (pa & PG_FRAME) | pte_prot(pmap, prot) | PG_V;
	npte |= (*(int *)pte & (PG_M|PG_U));
	if (wired)
		npte |= PG_W;
	if(va < UPT_MIN_ADDRESS)
		npte |= PG_u;
	else if(va < UPT_MAX_ADDRESS)
		npte |= PG_u | PG_RW;
	/*if (!checkpv && !cacheable)
		npte |= PG_CI;*/
#ifdef DEBUG
	if (pmapdebug & PDB_ENTER)
		printf("enter: new pte value %x\n", npte);
#endif
	ix = 0;
	do {
		*(int *)pte++ = npte;
		/*TBIS(va);*/
		npte += I386_PAGE_SIZE;
		va += I386_PAGE_SIZE;
	} while (++ix != i386pagesperpage);
	pte--;
#ifdef DEBUGx
	else if (pmapvacflush & PVF_ENTER) {
		if (pmapvacflush & PVF_TOTAL)
			DCIA();
		else if (pmap == kernel_pmap)
			DCIS();
		else
			DCIU();
	}
	if ((pmapdebug & PDB_WIRING) && pmap != kernel_pmap) {
		va -= PAGE_SIZE;
		pmap_check_wiring("enter", trunc_page(pmap_pte(pmap, va)));
	}
/* note: during vm_mem_init(), is called before u.u_procp et al is set up */
	if (pmap == u.u_procp->p_map->pmap)
		pmap_activate(pmap, (struct pcb *)u.u_procp->p_addr);
#endif
	load_cr3(u.u_pcb.pcb_ptd);
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
	register int ix;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		pg("pmap_change_wiring(%x, %x, %x)\n", pmap, va, wired);
#endif
	if (pmap == PMAP_NULL)
		return;

	pte = pmap_pte(pmap, va);
#ifdef DEBUG
	/*
	 * Page table page is not allocated.
	 * Should this ever happen?  Ignore it for now,
	 * we don't want to force allocation of unnecessary PTE pages.
	 */
	if (!pmap_pde_v(pmap_pde(pmap, va))) {
		if (pmapdebug & PDB_PARANOIA)
			pg("pmap_change_wiring: invalid PDE for %x\n", va);
		return;
	}
	/*
	 * Page not valid.  Should this ever happen?
	 * Just continue and change wiring anyway.
	 */
	if (!pmap_pte_v(pte)) {
		if (pmapdebug & PDB_PARANOIA)
			pg("pmap_change_wiring: invalid PTE for %x\n", va);
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
	ix = 0;
	do {
		pmap_pte_set_w(pte++, wired);
	} while (++ix != i386pagesperpage);
}

/*
 *	Routine:	pmap_pte
 *	Function:
 *		Extract the page table entry associated
 *		with the given map/virtual_address pair.
 */

struct pte *pmap_pte(pmap, va)
	register pmap_t	pmap;
	vm_offset_t va;
{

#ifdef DEBUGx
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_pte(%x, %x) ->\n", pmap, va);
#endif
	if (pmap && pmap_pde_v(pmap_pde(pmap, va))) {

		/* are we current address space or kernel? */
		if (*(int *)(pmap->pm_pdir+PTDPTDI) == *(int *)&PTDpde
			|| pmap == kernel_pmap)
			return ((struct pte *) vtopte(va));

		/* otherwise, we are alternate address space */
		else {
			if (*(int *)(pmap->pm_pdir+PTDPTDI)
				!= *(int *)&APTDpde) {
				APTDpde = pmap->pm_pdir[PTDPTDI];
/*pg("reload APT %x", *(int *) &APTDpde);*/
				load_cr3(u.u_pcb.pcb_ptd);
			}
			return((struct pte *) avtopte(va));
		}
	}
	return(0);
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

#ifdef DEBUGx
	if (pmapdebug & PDB_FOLLOW)
		pg("pmap_extract(%x, %x) -> ", pmap, va);
#endif
	pa = 0;
	if (pmap && pmap_pde_v(pmap_pde(pmap, va))) {
#ifdef notyet
		/* are we current address space or kernel? */
		if (pmap->pm_pdir[PTDPTDI] == PTDpde || pmap == kernel_pmap)
			pa = *(int *) vtopte(va);
#ifndef notyet
		else {	/* we are alternate address space */
			if (pmap->pm_pdir[PTDPTDI] != APTDpde) {
				APTDpde = pmap->pm_pdir[PTDPTDI];
/*pg("reload APT %x", *(int *) APTDpde);*/
				load_cr3(u.u_pcb.pcb_ptd);
			}
			pa = *(int *) avtopte(va);
		}
#else
		else {	/* extract pte from physical page table */
			*(int *)CMAP1 = (int) pmap_pde(pmap, va);
pg("other space %x", *(int *) CMAP1);
			load_cr3(u.u_pcb.pcb_ptd);
			pa = *(int *) CADDR1 [i386_btop(va) & 1023];
		}
#endif
#else
		pa = *(int *) pmap_pte(pmap, va);
#endif
	}
	if (pa)
		pa = (pa & PG_FRAME) | (va & ~PG_FRAME);
#ifdef DEBUGx
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
		pg("pmap_update()\n");
#endif
	/*TBIA(); */
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
	int *pde;
	int opmapdebug;
#endif
	if (pmap != kernel_pmap)
		return;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		pg("pmap_collect(%x)\n", pmap);
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
			if (pv->pv_ptpde && pv->pv_ptpmap == kernel_pmap)
				break;
		} while (pv = pv->pv_next);
		if (pv == PV_ENTRY_NULL)
			continue;
#ifdef DEBUGx
		if (pv->pv_va < (vm_offset_t)Sysmap ||
		    pv->pv_va >= (vm_offset_t)Sysmap + I386_MAX_PTSIZE)
			pg("collect: kernel PT VA out of range\n");
		else
			goto ok;
		pmap_pvdump(pa);
		continue;
ok:
#endif
		pte = (int *)(pv->pv_va + I386_PAGE_SIZE);
		while (--pte >= (int *)pv->pv_va && *pte == 0)
			;
		if (pte >= (int *)pv->pv_va)
			continue;

#ifdef DEBUG
		if (pmapdebug & (PDB_PTPAGE|PDB_COLLECT)) {
			pg("collect: freeing KPT page at %x (pde %x@%x)\n",
			       pv->pv_va, *(int *)pv->pv_ptpde, pv->pv_ptpde);
			opmapdebug = pmapdebug;
			pmapdebug |= PDB_PTPAGE;
		}

		pde = (int *)pv->pv_ptpde;
#endif
		/*
		 * If all entries were invalid we can remove the page.
		 * We call pmap_remove to take care of invalidating PD
		 * and Sysptmap entries.
		 */
		kpa = pmap_extract(pmap, pv->pv_va);
		pmap_remove(pmap, pv->pv_va, pv->pv_va + I386_PAGE_SIZE);
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
			pg("collect: %x (%x) to free list\n",
			       kpt->kpt_va, kpa);
#endif
		*pkpt = kpt->kpt_next;
		kpt->kpt_next = kpt_free_list;
		kpt_free_list = kpt;
#ifdef DEBUGx
		kpt_stats.kptinuse--;
		kpt_stats.collectpages++;
		if (pmapdebug & (PDB_PTPAGE|PDB_COLLECT))
			pmapdebug = opmapdebug;

		if (*pde)
			pg("collect: kernel PDE at %x still valid (%x)\n",
			       pde, *pde);
		pde = (int *)&Sysptmap[(pd_entry_t *)pde-pmap_pde(kernel_pmap, 0)];
		if (*pde)
			pg("collect: kernel PTmap at %x still valid (%x)\n",
			       pde, *pde);
#endif
	}
	splx(s);
}

void
pmap_activate(pmap, pcbp)
	register pmap_t pmap;
	struct pcb *pcbp;
{
int x;
#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_PDRTAB))
		printf("pmap_activate(%x, %x) ", pmap, pcbp);
#endif
	PMAP_ACTIVATE(pmap, pcbp);
/*printf("pde ");
for(x=0x3f6; x < 0x3fA; x++)
	printf("%x ", pmap->pm_pdir[x]);*/
/*pads(pmap);
pg(" pcb_cr3 %x", pcbp->pcb_cr3);*/
}

/*
 *	Routine:	pmap_kernel
 *	Function:
 *		Returns the physical map handle for the kernel.
 */
pmap_t
pmap_kernel()
{
    	return (kernel_pmap);
}

/*
 *	pmap_zero_page zeros the specified (machine independent)
 *	page by mapping the page into virtual memory and using
 *	bzero to clear its contents, one machine dependent page
 *	at a time.
 */
pmap_zero_page(phys)
	register vm_offset_t	phys;
{
	register int ix;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		printf("pmap_zero_page(%x)\n", phys);
#endif
	phys >>= PG_SHIFT;
	ix = 0;
	do {
		clearseg(phys++);
	} while (++ix != i386pagesperpage);
}

/*
 *	pmap_copy_page copies the specified (machine independent)
 *	page by mapping the page into virtual memory and using
 *	bcopy to copy the page, one machine dependent page at a
 *	time.
 */
pmap_copy_page(src, dst)
	register vm_offset_t	src, dst;
{
	register int ix;

#ifdef DEBUG
	if (pmapdebug & PDB_FOLLOW)
		pg("pmap_copy_page(%x, %x)\n", src, dst);
#endif
	src >>= PG_SHIFT;
	dst >>= PG_SHIFT;
	ix = 0;
	do {
		physcopyseg(src++, dst++);
	} while (++ix != i386pagesperpage);
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
			pg("pmap_pageable(%x, %x, %x, %x)\n",
			       pmap, sva, eva, pageable);
#endif
		if (!pmap_pde_v(pmap_pde(pmap, sva)))
			return;
		pa = pmap_pte_pa(pmap_pte(pmap, sva));
		if (pa < vm_first_phys || pa >= vm_last_phys)
			return;
		pv = pa_to_pvh(pa);
		if (pv->pv_ptpde == PD_ENTRY_NULL)
			return;
#ifdef DEBUG
		if (pv->pv_va != sva || pv->pv_next) {
			pg("pmap_pageable: bad PT page va %x next %x\n",
			       pv->pv_va, pv->pv_next);
			return;
		}
#endif
		/*
		 * Mark it unmodified to avoid pageout
		 */
		pmap_clear_modify(pa);
#ifdef DEBUG
		if (pmapdebug & PDB_PTPAGE)
			pg("pmap_pageable: PT page %x(%x) unmodified\n",
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
		pg("pmap_clear_modify(%x)\n", pa);
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
		pg("pmap_clear_reference(%x)\n", pa);
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
		pg("pmap_is_referenced(%x) -> %c\n", pa, "FT"[rv]);
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
		pg("pmap_is_modified(%x) -> %c\n", pa, "FT"[rv]);
		return(rv);
	}
#endif
	return(pmap_testbit(pa, PG_M));
}

vm_offset_t
pmap_phys_address(ppn)
	int ppn;
{
	return(i386_ptob(ppn));
}

/*
 * Miscellaneous support routines follow
 */

/* static */
i386_protection_init()
{
	register int *kp, prot;

	kp = protection_codes;
	for (prot = 0; prot < 8; prot++) {
		switch (prot) {
		case VM_PROT_NONE | VM_PROT_NONE | VM_PROT_NONE:
			*kp++ = 0;
			break;
		case VM_PROT_READ | VM_PROT_NONE | VM_PROT_NONE:
		case VM_PROT_READ | VM_PROT_NONE | VM_PROT_EXECUTE:
		case VM_PROT_NONE | VM_PROT_NONE | VM_PROT_EXECUTE:
			*kp++ = PG_RO;
			break;
		case VM_PROT_NONE | VM_PROT_WRITE | VM_PROT_NONE:
		case VM_PROT_NONE | VM_PROT_WRITE | VM_PROT_EXECUTE:
		case VM_PROT_READ | VM_PROT_WRITE | VM_PROT_NONE:
		case VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE:
			*kp++ = PG_RW;
			break;
		}
	}
}

/* static */
boolean_t
pmap_testbit(pa, bit)
	register vm_offset_t pa;
	int bit;
{
	register pv_entry_t pv;
	register int *pte, ix;
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
	if (pv->pv_pmap != PMAP_NULL) {
		for (; pv; pv = pv->pv_next) {
			pte = (int *) pmap_pte(pv->pv_pmap, pv->pv_va);
			ix = 0;
			do {
				if (*pte++ & bit) {
					splx(s);
					return(TRUE);
				}
			} while (++ix != i386pagesperpage);
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
	register int *pte, npte, ix;
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
	if (pv->pv_pmap != PMAP_NULL) {
#ifdef DEBUG
		int toflush = 0;
#endif
		for (; pv; pv = pv->pv_next) {
#ifdef DEBUG
			toflush |= (pv->pv_pmap == kernel_pmap) ? 2 : 1;
#endif
			va = pv->pv_va;
			pte = (int *) pmap_pte(pv->pv_pmap, va);
			ix = 0;
			do {
				if (setem)
					npte = *pte | bit;
				else
					npte = *pte & ~bit;
				if (*pte != npte) {
					*pte = npte;
					/*TBIS(va);*/
				}
				va += I386_PAGE_SIZE;
				pte++;
			} while (++ix != i386pagesperpage);

			if (pv->pv_pmap == u.u_procp->p_map->pmap)
				pmap_activate(pv->pv_pmap, (struct pcb *)u.u_procp->p_addr);
		}
#ifdef DEBUGx
		if (setem && bit == PG_RO && (pmapvacflush & PVF_PROTECT)) {
			if ((pmapvacflush & PVF_TOTAL) || toflush == 3)
				DCIA();
			else if (toflush == 2)
				DCIS();
			else
				DCIU();
		}
#endif
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
	pd_entry_t *pde;
	int s;

#ifdef DEBUG
	if (pmapdebug & (PDB_FOLLOW|PDB_ENTER|PDB_PTPAGE))
		pg("pmap_enter_ptpage: pmap %x, va %x\n", pmap, va);
	enter_stats.ptpneeded++;
#endif

	pde = pmap_pde(pmap, va);
	va = trunc_page((vm_offset_t)pmap_pte(pmap, va));

	/*
	 * In the kernel we allocate a page from the kernel PT page
	 * free list and map it into the kernel page table map (via
	 * pmap_enter).
	 */
	if (pmap == kernel_pmap) {

panic("pmap_enterptpage: kernel map");
#ifdef notquiteyet
		register struct kpt_page *kpt;

		s = splimp();
		if ((kpt = kpt_free_list) == (struct kpt_page *)0) {
			/*
			 * No PT pages available.
			 * Try once to free up unused ones.
			 */
#ifdef DEBUG
			if (pmapdebug & PDB_COLLECT)
				pg("enter: no KPT pages, collecting...\n");
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
		bzero(kpt->kpt_va, I386_PAGE_SIZE);
		pmap_enter(pmap, va, ptpa, VM_PROT_DEFAULT, TRUE);
#ifdef DEBUGx
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE))
			pg("enter: add &Sysptmap[%d]: %x (KPT page %x)\n",
			       pde - pmap_pde(pmap, 0),
			       *(int *)&Sysptmap[pde - pmap_pde(pmap, 0)],
			       kpt->kpt_va);
#endif
		splx(s);
#endif
	}
	/*
	 * For user processes we just simulate a fault on that location
	 * letting the VM system allocate a zero-filled page.
	 */
	else {
		vm_page_t	mem; vm_offset_t	offset;

#ifdef DEBUG
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE))
			pg("enter: about to load UPT pg at %x\n", va);
#endif
#ifdef needtotranslate
		offset = pdei(va);
		vm_object_lock((vm_object_t)pmap->pm_ptobj);

		while ((mem = vm_page_alloc((vm_object_t)pmap->pm_ptobj, offset))
			    == VM_PAGE_NULL) {
			vm_object_unlock((vm_object_t)pmap->pm_ptobj);
			VM_WAIT;
			vm_object_lock((vm_object_t)pmap->pm_ptobj);
		}
		vm_page_zero_fill(mem);
		mem->busy = FALSE;
		mem->ptpage = TRUE;
		ptpa = mem->phys_addr;
	
		vm_object_unlock((vm_object_t)pmap->pm_ptobj);
#endif
	}

	/*
	 * Locate the PV entry in the kernel for this PT page and
	 * record the PDE address.  This is so that we can invalidate
	 * the PDE when we remove the mapping for the page.
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
	if (pv == PV_ENTRY_NULL)
		panic("pmap_enter_ptpage: PT page not entered");
#endif
	pv->pv_ptpde = pde;
	pv->pv_ptpmap = pmap;
#ifdef DEBUG
	if (pmapdebug & (PDB_ENTER|PDB_PTPAGE))
		pg("enter: new PT page at PA %x, pde at %x\n", ptpa, pde);
#endif

	/*
	 * Map the new PT page into the page directory table.
	 *Also increment the reference count on the page directory table if this
	 * was a user page table page.  Note that we don't use vm_map_pageable
	 * to keep the count like we do for PT pages, this is mostly because
	 * it would be difficult to identify PD pages in pmap_pageable to
	 * release them.  We also avoid the overhead of vm_map_pageable.
	 */
	*(int *)pde = (ptpa & PG_FRAME) | PG_RW | PG_V;
	if (pmap != kernel_pmap) {
		pmap->pm_dref++;
#ifdef DEBUG
		if (pmapdebug & (PDB_ENTER|PDB_PTPAGE|PDB_PDRTAB))
			pg("enter: pdtab %x refcnt %d\n",
			       pmap->pm_pdir, pmap->pm_dref);
#endif
	}
	/*
	 * Flush stale TLB info.
	 */
	/*if (pmap == kernel_pmap)
		TBIAS();
	else
		TBIAU(); */
	if (pmap == u.u_procp->p_map->pmap)
		pmap_activate(pmap, (struct pcb *)u.u_procp->p_addr);
	pmap->pm_ptpages++;
	splx(s);
}

#ifdef DEBUG
pmap_pvdump(pa)
	vm_offset_t pa;
{
	register pv_entry_t pv;

	pg("pa %x", pa);
	for (pv = pa_to_pvh(pa); pv; pv = pv->pv_next)
		pg(" -> pmap %x, va %x, ptpde %x, ptpmap %x, flags %x",
		       pv->pv_pmap, pv->pv_va, pv->pv_ptpde, pv->pv_ptpmap,
		       pv->pv_flags);
	pg("\n");
}

pmap_check_wiring(str, va)
	char *str;
	vm_offset_t va;
{
	vm_map_entry_t entry;
	register int count, *pte;

	va = trunc_page(va);
	if (!pmap_pde_v(pmap_pde(kernel_pmap, va)) ||
	    !pmap_pte_v(pmap_pte(kernel_pmap, va)))
		return;

	if (!vm_map_lookup_entry(pt_map, va, &entry)) {
		pg("wired_check: entry for %x not found\n", va);
		return;
	}
	count = 0;
	for (pte = (int *)va; pte < (int *)(va+PAGE_SIZE); pte++)
		if (*pte)
			count++;
	if (entry->wired_count != count)
		pg("*%s*: %x: w%d/a%d\n",
		       str, va, entry->wired_count, count);
}

/* print address space of pmap*/
pads(pm) pmap_t pm; {
	unsigned va, i, j;
	struct pte *ptep;

	for (i = 0; i < 1024; i++) 
		if(pm->pm_pdir[i].pd_v)
			for (j = 0; j < 1024 ; j++) {
				va = (i<<22)+(j<<12);
				if (pm == kernel_pmap && va < 0xfe000000)
						continue;
				if (pm != kernel_pmap && va > 0xfdc00000)
						continue;
				ptep = pmap_pte(pm, va);
				if(pmap_pte_v(ptep)) 
					printf("%x:%x ", va, *(int *)ptep); 
			} ;
				
}
#endif
