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
 *	OMRON: $Id: pmap_bootstrap.c,v 1.2 92/06/14 18:11:27 moti Exp $
 *
 * from: hp300/hp300/pmap_bootstrap.c	7.1 (Berkeley) 6/5/92
 *
 *	@(#)pmap_bootstrap.c	7.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <luna68k/luna68k/pte.h>
#include <machine/vmparam.h>
#include <machine/cpu.h>

#include <vm/vm.h>

/*
 * Allocate various and sundry SYSMAPs used in the days of old VM
 * and not yet converted.  XXX.
 */
#define BSDVM_COMPAT	1

extern char *etext;
extern int Sysptsize;

extern char *proc0paddr;
extern struct ste *Sysseg;
extern struct pte *Sysptmap, *Sysmap;
extern vm_offset_t Umap;

extern int maxmem, physmem;
extern vm_offset_t avail_start, avail_end, virtual_avail, virtual_end;
extern vm_size_t mem_size;
extern int protection_codes[];
#if defined(DYNPGSIZE)
extern int lunapagesperpage;
#endif

#if BSDVM_COMPAT
#include <sys/msgbuf.h>

/*
 * All those kernel PT submaps that BSD is so fond of
 */
struct pte	*CMAP1, *CMAP2, *mmap;
caddr_t		CADDR1, CADDR2, vmmap;
struct pte	*msgbufmap;
struct msgbuf	*msgbufp;
#endif

/* 
 * LUNA H/W information.
 */
struct physmap io_physmap[] =
{
	{0x40000000,0x00100000,1},	/* debugger */
	{0x41000000,0x00020000,1},	/* PROM */
	{0x45000000,0x00000800,0},	/* calendar clock */
	{0x49000000,0x00000004,0},	/* pio-0 */
	{0x4D000000,0x00000004,0},	/* pio-1 */
	{0x51000000,0x00000008,0},	/* sio */
	{0x61000000,0x00000001,0},	/* TAS register */
	{0x63000000,0x00000001,0},	/* SYSINT flag */
	{0x6B000000,0x00000001,0},	/* internal FPP enable/disable */
	{0x6F000000,0x00000001,0},	/* external FPP enable/disable */
	{0x71000000,0x00020000,0},	/* 3 port RAM */
	{0,0,0}				/* terminate */
};
#define	IO_DBG_OFF	0		/* debugger offset in io_physmap[] */
#define	IOPTPAGE	((sizeof(io_physmap)/sizeof(struct physmap))-1)
int	ioptpage = IOPTPAGE;		/* for locore */

/*
 * Bootstrap the VM system.
 *
 * Called with MMU off so we must relocate all global references by `firstpa'
 * (don't call any functions here!)  `nextpa' is the first available physical
 * memory address.  Returns an updated first PA reflecting the memory we
 * have allocated.  MMU is still off when we return.
 *
 * XXX assumes sizeof(u_int) == sizeof(struct pte)
 * XXX a PIC compiler would make this much easier.
 */
void
pmap_bootstrap(nextpa, firstpa)
	vm_offset_t nextpa;
	register vm_offset_t firstpa;
{
	vm_offset_t kstpa, kptpa, iopa, kptmpa, ukptpa, p0upa;
	u_int nptpages, kstsize;
	register u_int protoste, protopte, *ste, *pte, *epte;

	/*
	 * Calculate important physical addresses:
	 *
	 *	kstpa		kernel segment table	1 page (!040)
	 *						N pages (040)
	 *
	 *	kptpa		statically allocated
	 *			kernel PT pages		Sysptsize+ pages
	 *
	 *	kptmpa		kernel PT map		1 page
	 *
	 *	ukptpa		Uarea kernel PT page	1 page
	 *
	 *	iopa		IO and debbuger space
	 *			PT pages		IOPTPAGE pages
	 *
	 *
	 *	p0upa		proc 0 u-area		UPAGES pages
	 *
	 * The KVA corresponding to any of these PAs is:
	 *	(PA - firstpa + KERNBASE).
	 */
	kstsize = 1;
	kstpa = nextpa;
	nextpa += kstsize * NBPG;
	kptpa = nextpa;
	nptpages = Sysptsize;
	nextpa += nptpages * NBPG;
	kptmpa = nextpa;
	nextpa += NBPG;
	ukptpa = nextpa;
	nextpa += NBPG;
	iopa = nextpa;
	nextpa += IOPTPAGE * NBPG;
	p0upa = nextpa;
	nextpa += UPAGES * NBPG;

	/*
	 * Initialize segment table and kernel page table map.
	 *
	 * On 68030s and earlier MMUs the two are identical except for
	 * the valid bits so both are initialized with essentially the
	 * same values.
	 * 0x3FF00000 for UPAGES is used for mapping the current process u-area
	 * (u + kernel stack). 
	 */

	/*
	 * Map the page table pages in both the HW segment table
	 * and the software Sysptmap.  Note that Sysptmap is also
	 * considered a PT page hence the +1.
	 */
	ste = (u_int *)kstpa;
	pte = (u_int *)kptmpa;
	epte = &pte[nptpages+1];
	protoste = kptpa | SG_RW | SG_V;
	protopte = kptpa | PG_RW | PG_CI | PG_V;
	while (pte < epte) {
	    *ste++ = protoste;
	    *pte++ = protopte;
	    protoste += NBPG;
	    protopte += NBPG;
	}
	/*
	 * Invalidate all but the last remaining entries in both.
	 */
	epte = &((u_int *)kptmpa)[NPTEPG];
	while (pte < epte) {
	    *ste++ = SG_NV;
	    *pte++ = PG_NV;
	}
	/* LUNA: Uarea pt map */
	ste = (u_int *)kstpa;
	pte = (u_int *)kptmpa;
	ste[KERNELSTACK>>SG_ISHIFT] = ukptpa | SG_RW | SG_V;
	pte[KERNELSTACK>>SG_ISHIFT] = ukptpa | PG_RW | PG_CI | PG_V;

	/*
	 * Invalidate all but the final entry in the last kernel PT page
	 * (u-area PTEs will be validated later).  The final entry maps
	 * the last page of physical memory.
	 */
	pte = (u_int *)ukptpa;
	epte = &pte[NPTEPG];
	while (pte < epte)
		*pte++ = PG_NV;
	/*
	 * Initialize kernel page table.
	 * Start by invalidating the `nptpages' that we have allocated.
	 */
	pte = (u_int *)kptpa;
	epte = &pte[nptpages * NPTEPG];
	while (pte < epte)
		*pte++ = PG_NV;
	/*
	 * Validate PTEs for kernel text (RO)
	 */
	pte = &((u_int *)kptpa)[luna_btop(KERNBASE)];
	epte = &pte[luna_btop(luna_trunc_page(&etext))];
#ifdef KGDB
	protopte = firstpa | PG_RW | PG_V;	/* XXX RW for now */
#else
	protopte = firstpa | PG_RO | PG_V;
#endif
	while (pte < epte) {
		*pte++ = protopte;
		protopte += NBPG;
	}
	/*
	 * Validate PTEs for kernel data/bss, dynamic data allocated
	 * by us so far (nextpa - firstpa bytes), and pages for proc0
	 * u-area and page table allocated below (RW).
	 */
	epte = &((u_int *)kptpa)[luna_btop(nextpa - firstpa)];
	protopte = (protopte & ~PG_PROT) | PG_RW;
	while (pte < epte) {
		*pte++ = protopte;
		protopte += NBPG;
	}

	/* initialize; all IO pte invalidate */
	pte = (u_int *)iopa;
	epte = &pte[IOPTPAGE * NPTEPG];
	while (pte < epte)
		*pte++ = PG_NV;
	/*
	 * Here, we validate STEs and kernel page table PTEs
	 * for io space.
	 */
	{
	    int index;

	    protoste = iopa | SG_RW | SG_V;
	    protopte = iopa | PG_RW | PG_CI | PG_V;
	    for (index = 0; io_physmap[index].pm_phys; index++)
	      {
		  ste = &((u_int *)kstpa)[io_physmap[index].pm_phys/NBSEG];
		  pte = &((u_int *)kptmpa)[io_physmap[index].pm_phys/NBSEG];
		  *ste = protoste;
		  *pte = protopte;
		  protoste += NBPG;
		  protopte += NBPG;
	      }
	    /*
	     * Finally, validate the IO space PTEs.
	     */
	    /* create io(and debbuger) PTEs */
	    for (index = 0; io_physmap[index].pm_phys; index++)
	      {
		  pte = (u_int *)iopa + index*NPTEPG;
		  epte = &pte[(luna_round_page(io_physmap[index].pm_size))>>PG_SHIFT];
		  /* 
		   * First entry(index == IO_DBG_OFF) is very special, 
		   * we map debugger at fixed address(0x40000000).
		   * Debugger is always loaded (maxmem+1) page.
		   */
		  protopte = (index == IO_DBG_OFF ? 
			      ((maxmem+1)<<PG_SHIFT) : io_physmap[index].pm_phys) |
		    PG_RW |(io_physmap[index].pm_cache == 0 ? PG_CI : 0) | PG_V;
		  
		  /* physical page setup loop */
		  while (pte < epte) {
		      *pte++ = protopte;
		      protopte += NBPG;
		  }
	      }
	}
	/*
	 * Calculate important exported kernel virtual addresses
	 */
	/*
	 * Sysseg: base of kernel segment table
	 */
	Sysseg = (struct ste *)(kstpa - firstpa);
	/*
	 * Sysptmap: base of kernel page table map
	 */
	Sysptmap = (struct pte *)(kptmpa - firstpa);
	/*
	 * Sysmap: kernel page table (as mapped through Sysptmap)
	 * Immediately follows `nptpages' of static kernel page table.
	 */
	Sysmap = (struct pte *)luna_ptob(nptpages * NPTEPG);
	/*
	 * Umap: first of UPAGES PTEs (in Sysmap) for fixed-address u-area.
	 * HIGHPAGES PTEs from the end of Sysmap.
	 * LUNA: User stack address = 0x3ff00000.
	 */
	Umap = (vm_offset_t)Sysmap + (LUNA_MAX_PTSIZE/4 - HIGHPAGES * sizeof(struct pte));
	/*
	 * Setup u-area for process 0.
	 */
	/*
	 * Validate PTEs in Sysmap corresponding to the u-area (Umap)
	 * which are HIGHPAGES from the end of the last kernel PT page
	 * allocated earlier.
	 */
	pte = &((u_int *)ukptpa)[NPTEPG - HIGHPAGES];
	epte = &pte[UPAGES];
	protopte = p0upa | PG_RW | PG_V;
	while (pte < epte) {
		*pte++ = protopte;
		protopte += NBPG;
	}
	/*
	 * Zero the u-area.
	 * NOTE: `pte' and `epte' aren't PTEs here.
	 */
	pte = (u_int *)p0upa;
	epte = (u_int *)(p0upa + UPAGES*NBPG);
	while (pte < epte)
		*pte++ = 0;
	/*
	 * Remember the u-area address so it can be loaded in the
	 * proc struct p_addr field later.
	 */
	proc0paddr = (char *)(p0upa - firstpa);

	/*
	 * VM data structures are now initialized, set up data for
	 * the pmap module.
	 */
	avail_start = nextpa;
	avail_end = luna_ptob(maxmem);
#if BSDVM_COMPAT
			/* XXX allow for msgbuf */
			- luna_round_page(sizeof(struct msgbuf))
#endif
				;
	mem_size = luna_ptob(physmem);
	virtual_avail =	VM_MIN_KERNEL_ADDRESS + (nextpa - firstpa);
	virtual_end = VM_MAX_KERNEL_ADDRESS;
#if defined(DYNPGSIZE)
	lunapagesperpage = 1;		/* XXX */
#endif
	/*
	 * Initialize protection array.
	 */
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

	/*
	 * Kernel page/segment table allocated in locore,
	 * just initialize pointers.
	 */
	{
		struct pmap *kpm = &kernel_pmap_store;

		kpm->pm_stab = Sysseg;
		kpm->pm_ptab = Sysmap;
		simple_lock_init(&kpm->pm_lock);
		kpm->pm_count = 1;
		kpm->pm_stpa = (struct ste *)kstpa;
	}

#if BSDVM_COMPAT
#define	SYSMAP(c, p, v, n) \
	v = (c)va; va += ((n)*LUNA_PAGE_SIZE); \
	p = (struct pte *)pte; pte += (n);

	/*
	 * Allocate all the submaps we need
	 */
	{
		vm_offset_t va = virtual_avail;

		pte = &Sysmap[luna_btop(va)];
	
		SYSMAP(caddr_t		,CMAP1		,CADDR1	   ,1	)
		SYSMAP(caddr_t		,CMAP2		,CADDR2	   ,1	)
		SYSMAP(caddr_t		,mmap		,vmmap	   ,1	)
		SYSMAP(struct msgbuf *	,msgbufmap	,msgbufp   ,1	)

		virtual_avail = va;
	}
#undef	SYSMAP
#endif
}

pmap_showstuff()
{
	int i;
	printf("CADDR1=%x pte at CMAP1=%x\n", CADDR1, CMAP1);
	printf("CADDR2=%x pte at CMAP2=%x\n", CADDR2, CMAP2);
	printf("vmmap=%x pte at mmap=%x\n", vmmap, mmap);
	printf("msgbufp=%x pte at msgbufmap=%x\n", msgbufp, msgbufmap);
	printf("virtual_avail=%x, virtual_end=%x\n", virtual_avail, virtual_end);
	for (i = 0; i < 8; i++)
		printf("%x ", protection_codes[i]);
	printf("\n");
}

#ifdef BOOTDEBUG
/*
 *	Bootstrap the system enough to run with virtual memory.
 *	Map the kernel's code and data, and allocate the system page table.
 *
 *	On the HP this is called after mapping has already been enabled
 *	and just syncs the pmap module with what has already been done.
 *	[We can't call it easily with mapping off since the kernel is not
 *	mapped with PA == VA, hence we would have to relocate every address
 *	from the linked base (virtual) address 0 to the actual (physical)
 *	address of 0xFFxxxxxx.]
 */
void
Opmap_bootstrap(firstaddr, loadaddr)
	vm_offset_t firstaddr;
	vm_offset_t loadaddr;
{
#if BSDVM_COMPAT
	vm_offset_t va;
	struct pte *pte;
#endif

	avail_start = firstaddr;
	avail_end = maxmem << PGSHIFT;

#if BSDVM_COMPAT
	/* XXX: allow for msgbuf */
	avail_end -= luna_round_page(sizeof(struct msgbuf));
#endif

	mem_size = physmem << PGSHIFT;
	virtual_avail = VM_MIN_KERNEL_ADDRESS + (firstaddr - loadaddr);
	virtual_end = VM_MAX_KERNEL_ADDRESS;
#if defined(DYNPGSIZE)
	lunapagesperpage = PAGE_SIZE / LUNA_PAGE_SIZE;
#endif
	/*
	 * Initialize protection array.
	 */
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
	/*
	 * Kernel page/segment table allocated in locore,
	 * just initialize pointers.
	 */
	kernel_pmap->pm_stab = Sysseg;
	kernel_pmap->pm_ptab = Sysmap;

	simple_lock_init(&kernel_pmap->pm_lock);
	kernel_pmap->pm_count = 1;

#if BSDVM_COMPAT
	/*
	 * Allocate all the submaps we need
	 */
#define	SYSMAP(c, p, v, n)	\
	v = (c)va; va += ((n)*LUNA_PAGE_SIZE); p = pte; pte += (n);

	va = virtual_avail;
	pte = &Sysmap[luna_btop(va)];

	SYSMAP(caddr_t		,CMAP1		,CADDR1	   ,1		)
	SYSMAP(caddr_t		,CMAP2		,CADDR2	   ,1		)
	SYSMAP(caddr_t		,mmap		,vmmap	   ,1		)
	SYSMAP(struct msgbuf *	,msgbufmap	,msgbufp   ,1		)
	virtual_avail = va;
#undef SYSMAP
#endif
}
#endif
