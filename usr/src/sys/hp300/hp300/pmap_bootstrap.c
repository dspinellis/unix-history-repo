/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmap_bootstrap.c	7.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/msgbuf.h>
#include <hp300/hp300/pte.h>
#include <hp300/hp300/clockreg.h>
#include <machine/vmparam.h>
#include <machine/cpu.h>

#include <vm/vm.h>

#define RELOC(v, t)	*((t*)((u_int)&(v) + firstpa))

extern char *etext;
extern int Sysptsize;
extern char *extiobase, *proc0paddr;
extern struct ste *Sysseg;
extern struct pte *Sysptmap, *Sysmap;
extern vm_offset_t Umap, CLKbase, MMUbase;

extern int maxmem, physmem;
extern vm_offset_t avail_start, avail_end, virtual_avail, virtual_end;
extern vm_size_t mem_size;
extern int protection_codes[];
#ifdef HAVEVAC
extern int pmap_aliasmask;
#endif

/*
 * Special purpose kernel virtual addresses, used for mapping
 * physical pages for a variety of temporary or permanent purposes:
 *
 *	CADDR1, CADDR2:	pmap zero/copy operations
 *	vmmap:		/dev/mem, crash dumps, parity error checking
 *	ledbase:	SPU LEDs
 *	msgbufp:	kernel message buffer
 */
caddr_t		CADDR1, CADDR2, vmmap, ledbase;
struct msgbuf	*msgbufp;

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
	vm_offset_t kstpa, kptpa, iiopa, eiopa, kptmpa, lkptpa, p0upa;
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
	 *	iiopa		internal IO space
	 *			PT pages		IIOMAPSIZE pages
	 *
	 *	eiopa		external IO space
	 *			PT pages		EIOMAPSIZE pages
	 *
	 * [ Sysptsize is the number of pages of PT, IIOMAPSIZE and
	 *   EIOMAPSIZE are the number of PTEs, hence we need to round
	 *   the total to a page boundary with IO maps at the end. ]
	 *
	 *	kptmpa		kernel PT map		1 page
	 *
	 *	lkptpa		last kernel PT page	1 page
	 *
	 *	p0upa		proc 0 u-area		UPAGES pages
	 *
	 * The KVA corresponding to any of these PAs is:
	 *	(PA - firstpa + KERNBASE).
	 */
	if (RELOC(mmutype, int) == MMU_68040)
		kstsize = MAXKL2SIZE / (NPTEPG/SG4_LEV2SIZE);
	else
		kstsize = 1;
	kstpa = nextpa;
	nextpa += kstsize * NBPG;
	kptpa = nextpa;
	nptpages = RELOC(Sysptsize, int) +
		(IIOMAPSIZE + EIOMAPSIZE + NPTEPG - 1) / NPTEPG;
	nextpa += nptpages * NBPG;
	eiopa = nextpa - EIOMAPSIZE * sizeof(struct pte);
	iiopa = eiopa - IIOMAPSIZE * sizeof(struct pte);
	kptmpa = nextpa;
	nextpa += NBPG;
	lkptpa = nextpa;
	nextpa += NBPG;
	p0upa = nextpa;
	nextpa += UPAGES * NBPG;

	/*
	 * Initialize segment table and kernel page table map.
	 *
	 * On 68030s and earlier MMUs the two are identical except for
	 * the valid bits so both are initialized with essentially the
	 * same values.  On the 68040, which has a mandatory 3-level
	 * structure, the segment table holds the level 1 table and part
	 * (or all) of the level 2 table and hence is considerably
	 * different.  Here the first level consists of 128 descriptors
	 * (512 bytes) each mapping 32mb of address space.  Each of these
	 * points to blocks of 128 second level descriptors (512 bytes)
	 * each mapping 256kb.  Note that there may be additional "segment
	 * table" pages depending on how large MAXKL2SIZE is.
	 *
	 * Portions of the last segment of KVA space (0xFFF00000 -
	 * 0xFFFFFFFF) are mapped for a couple of purposes.  0xFFF00000
	 * for UPAGES is used for mapping the current process u-area
	 * (u + kernel stack).  The very last page (0xFFFFF000) is mapped
	 * to the last physical page of RAM to give us a region in which
	 * PA == VA.  We use the first part of this page for enabling
	 * and disabling mapping.  The last part of this page also contains
	 * info left by the boot ROM.
	 *
	 * XXX cramming two levels of mapping into the single "segment"
	 * table on the 68040 is intended as a temporary hack to get things
	 * working.  The 224mb of address space that this allows will most
	 * likely be insufficient in the future (at least for the kernel).
	 */
	if (RELOC(mmutype, int) == MMU_68040) {
		register int num;

		/*
		 * First invalidate the entire "segment table" pages
		 * (levels 1 and 2 have the same "invalid" value).
		 */
		pte = (u_int *)kstpa;
		epte = &pte[kstsize * NPTEPG];
		while (pte < epte)
			*pte++ = SG_NV;
		/*
		 * Initialize level 2 descriptors (which immediately
		 * follow the level 1 table).  We need:
		 *	NPTEPG / SG4_LEV3SIZE
		 * level 2 descriptors to map each of the nptpages+1
		 * pages of PTEs.  Note that we set the "used" bit
		 * now to save the HW the expense of doing it.
		 */
		num = (nptpages + 1) * (NPTEPG / SG4_LEV3SIZE);
		pte = &((u_int *)kstpa)[SG4_LEV1SIZE];
		epte = &pte[num];
		protoste = kptpa | SG_U | SG_RW | SG_V;
		while (pte < epte) {
			*pte++ = protoste;
			protoste += (SG4_LEV3SIZE * sizeof(struct ste));
		}
		/*
		 * Initialize level 1 descriptors.  We need:
		 *	roundup(num, SG4_LEV2SIZE) / SG4_LEV2SIZE
		 * level 1 descriptors to map the `num' level 2's.
		 */
		pte = (u_int *)kstpa;
		epte = &pte[roundup(num, SG4_LEV2SIZE) / SG4_LEV2SIZE];
		protoste = (u_int)&pte[SG4_LEV1SIZE] | SG_U | SG_RW | SG_V;
		while (pte < epte) {
			*pte++ = protoste;
			protoste += (SG4_LEV2SIZE * sizeof(struct ste));
		}
		/*
		 * Initialize the final level 1 descriptor to map the last
		 * block of level 2 descriptors.
		 */
		ste = &((u_int *)kstpa)[SG4_LEV1SIZE-1];
		pte = &((u_int *)kstpa)[kstsize*NPTEPG - SG4_LEV2SIZE];
		*ste = (u_int)pte | SG_U | SG_RW | SG_V;
		/*
		 * Now initialize the final portion of that block of
		 * descriptors to map the "last PT page".
		 */
		pte = &((u_int *)kstpa)[kstsize*NPTEPG - NPTEPG/SG4_LEV3SIZE];
		epte = &pte[NPTEPG/SG4_LEV3SIZE];
		protoste = lkptpa | SG_U | SG_RW | SG_V;
		while (pte < epte) {
			*pte++ = protoste;
			protoste += (SG4_LEV3SIZE * sizeof(struct ste));
		}
		/*
		 * Initialize Sysptmap
		 */
		pte = (u_int *)kptmpa;
		epte = &pte[nptpages+1];
		protopte = kptpa | PG_RW | PG_CI | PG_V;
		while (pte < epte) {
			*pte++ = protopte;
			protopte += NBPG;
		}
		pte = &((u_int *)kptmpa)[NPTEPG-1];
		*pte = lkptpa | PG_RW | PG_CI | PG_V;
	} else {
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
		epte = &((u_int *)kptmpa)[NPTEPG-1];
		while (pte < epte) {
			*ste++ = SG_NV;
			*pte++ = PG_NV;
		}
		/*
		 * Initialize the last to point to point to the page
		 * table page allocated earlier.
		 */
		*ste = lkptpa | SG_RW | SG_V;
		*pte = lkptpa | PG_RW | PG_CI | PG_V;
	}
	/*
	 * Invalidate all but the final entry in the last kernel PT page
	 * (u-area PTEs will be validated later).  The final entry maps
	 * the last page of physical memory.
	 */
	pte = (u_int *)lkptpa;
	epte = &pte[NPTEPG-1];
	while (pte < epte)
		*pte++ = PG_NV;
	*pte = MAXADDR | PG_RW | PG_CI | PG_V;
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
	pte = &((u_int *)kptpa)[hp300_btop(KERNBASE)];
	epte = &pte[hp300_btop(hp300_trunc_page(&etext))];
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
	epte = &((u_int *)kptpa)[hp300_btop(nextpa - firstpa)];
	protopte = (protopte & ~PG_PROT) | PG_RW;
	/*
	 * Enable copy-back caching of data pages
	 */
	if (RELOC(mmutype, int) == MMU_68040)
		protopte |= PG_CCB;
	while (pte < epte) {
		*pte++ = protopte;
		protopte += NBPG;
	}
	/*
	 * Finally, validate the internal IO space PTEs (RW+CI).
	 * We do this here since the 320/350 MMU registers (also
	 * used, but to a lesser extent, on other models) are mapped
	 * in this range and it would be nice to be able to access
	 * them after the MMU is turned on.
	 */
	pte = (u_int *)iiopa;
	epte = (u_int *)eiopa;
	protopte = INTIOBASE | PG_RW | PG_CI | PG_V;
	while (pte < epte) {
		*pte++ = protopte;
		protopte += NBPG;
	}

	/*
	 * Calculate important exported kernel virtual addresses
	 */
	/*
	 * Sysseg: base of kernel segment table
	 */
	RELOC(Sysseg, struct ste *) =
		(struct ste *)(kstpa - firstpa);
	/*
	 * Sysptmap: base of kernel page table map
	 */
	RELOC(Sysptmap, struct pte *) =
		(struct pte *)(kptmpa - firstpa);
	/*
	 * Sysmap: kernel page table (as mapped through Sysptmap)
	 * Immediately follows `nptpages' of static kernel page table.
	 */
	RELOC(Sysmap, struct pte *) =
		(struct pte *)hp300_ptob(nptpages * NPTEPG);
	/*
	 * Umap: first of UPAGES PTEs (in Sysmap) for fixed-address u-area.
	 * HIGHPAGES PTEs from the end of Sysmap.
	 */
	RELOC(Umap, vm_offset_t) =
		(vm_offset_t)RELOC(Sysmap, struct pte *) +
			(HP_MAX_PTSIZE - HIGHPAGES * sizeof(struct pte));
	/*
	 * intiobase, intiolimit: base and end of internal (DIO) IO space.
	 * IIOMAPSIZE pages prior to external IO space at end of static
	 * kernel page table.
	 */
	RELOC(intiobase, char *) =
		(char *)hp300_ptob(nptpages*NPTEPG - (IIOMAPSIZE+EIOMAPSIZE));
	RELOC(intiolimit, char *) =
		(char *)hp300_ptob(nptpages*NPTEPG - EIOMAPSIZE);
	/*
	 * extiobase: base of external (DIO-II) IO space.
	 * EIOMAPSIZE pages at the end of the static kernel page table.
	 */
	RELOC(extiobase, char *) =
		(char *)hp300_ptob(nptpages*NPTEPG - EIOMAPSIZE);
	/*
	 * CLKbase, MMUbase: important registers in internal IO space
	 * accessed from assembly language.
	 */
	RELOC(CLKbase, vm_offset_t) =
		(vm_offset_t)RELOC(intiobase, char *) + CLKBASE;
	RELOC(MMUbase, vm_offset_t) =
		(vm_offset_t)RELOC(intiobase, char *) + MMUBASE;

	/*
	 * Setup u-area for process 0.
	 */
	/*
	 * Validate PTEs in Sysmap corresponding to the u-area (Umap)
	 * which are HIGHPAGES from the end of the last kernel PT page
	 * allocated earlier.
	 */
	pte = &((u_int *)lkptpa)[NPTEPG - HIGHPAGES];
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
	RELOC(proc0paddr, char *) = (char *)(p0upa - firstpa);

	/*
	 * VM data structures are now initialized, set up data for
	 * the pmap module.
	 */
	RELOC(avail_start, vm_offset_t) = nextpa;
	RELOC(avail_end, vm_offset_t) =
		hp300_ptob(RELOC(maxmem, int))
			/* XXX allow for msgbuf */
			- hp300_round_page(sizeof(struct msgbuf));
	RELOC(mem_size, vm_size_t) = hp300_ptob(RELOC(physmem, int));
	RELOC(virtual_avail, vm_offset_t) =
		VM_MIN_KERNEL_ADDRESS + (nextpa - firstpa);
	RELOC(virtual_end, vm_offset_t) = VM_MAX_KERNEL_ADDRESS;

#ifdef HAVEVAC
	/*
	 * Determine VA aliasing distance if any
	 */
	if (RELOC(ectype, int) == EC_VIRT)
		if (RELOC(machineid, int) == HP_320)
			RELOC(pmap_aliasmask, int) = 0x3fff;	/* 16k */
		else if (RELOC(machineid, int) == HP_350)
			RELOC(pmap_aliasmask, int) = 0x7fff;	/* 32k */
#endif

	/*
	 * Initialize protection array.
	 * XXX don't use a switch statement, it might produce an
	 * absolute "jmp" table.
	 */
	{
		register int *kp;

		kp = &RELOC(protection_codes, int);
		kp[VM_PROT_NONE|VM_PROT_NONE|VM_PROT_NONE] = 0;
		kp[VM_PROT_READ|VM_PROT_NONE|VM_PROT_NONE] = PG_RO;
		kp[VM_PROT_READ|VM_PROT_NONE|VM_PROT_EXECUTE] = PG_RO;
		kp[VM_PROT_NONE|VM_PROT_NONE|VM_PROT_EXECUTE] = PG_RO;
		kp[VM_PROT_NONE|VM_PROT_WRITE|VM_PROT_NONE] = PG_RW;
		kp[VM_PROT_NONE|VM_PROT_WRITE|VM_PROT_EXECUTE] = PG_RW;
		kp[VM_PROT_READ|VM_PROT_WRITE|VM_PROT_NONE] = PG_RW;
		kp[VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE] = PG_RW;
	}

	/*
	 * Kernel page/segment table allocated in locore,
	 * just initialize pointers.
	 */
	{
		struct pmap *kpm = &RELOC(kernel_pmap_store, struct pmap);

		kpm->pm_stab = RELOC(Sysseg, struct ste *);
		kpm->pm_ptab = RELOC(Sysmap, struct pte *);
		simple_lock_init(&kpm->pm_lock);
		kpm->pm_count = 1;
		kpm->pm_stpa = (struct ste *)kstpa;
		/*
		 * For the 040 we also initialize the free level 2
		 * descriptor mask noting that we have used:
		 *	0:		level 1 table
		 *	1 to `num':	map page tables
		 *	MAXKL2SIZE-1:	maps last-page page table
		 */
		if (RELOC(mmutype, int) == MMU_68040) {
			register int num;
			
			kpm->pm_stfree = ~l2tobm(0);
			num = roundup((nptpages + 1) * (NPTEPG / SG4_LEV3SIZE),
				      SG4_LEV2SIZE) / SG4_LEV2SIZE;
			while (num)
				kpm->pm_stfree &= ~l2tobm(num--);
			kpm->pm_stfree &= ~l2tobm(MAXKL2SIZE-1);
			for (num = MAXKL2SIZE;
			     num < sizeof(kpm->pm_stfree)*NBBY;
			     num++)
				kpm->pm_stfree &= ~l2tobm(num);
		}
	}

	/*
	 * Allocate some fixed, special purpose kernel virtual addresses
	 */
	{
		vm_offset_t va = RELOC(virtual_avail, vm_offset_t);

		RELOC(CADDR1, caddr_t) = (caddr_t)va;
		va += HP_PAGE_SIZE;
		RELOC(CADDR2, caddr_t) = (caddr_t)va;
		va += HP_PAGE_SIZE;
		RELOC(vmmap, caddr_t) = (caddr_t)va;
		va += HP_PAGE_SIZE;
		RELOC(ledbase, caddr_t) = (caddr_t)va;
		va += HP_PAGE_SIZE;
		RELOC(msgbufp, struct msgbuf *) = (struct msgbuf *)va;
		va += HP_PAGE_SIZE;
		RELOC(virtual_avail, vm_offset_t) = va;
	}
}
