/* 
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: hp300/hp300/pmap_bootstrap.c	7.4 (Berkeley) 12/27/92
 *
 *	@(#)pmap_bootstrap.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <sys/msgbuf.h>
#include <luna68k/luna68k/pte.h>
#include <machine/vmparam.h>
#include <machine/cpu.h>

#include <vm/vm.h>

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
	vm_offset_t kstpa, kptpa, kptmpa, lkptpa, p0upa;
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
	 * [ Sysptsize is the number of pages of PT, hence we need to
	 *   round the total to a page boundary at the end. ]
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
	if (mmutype == MMU_68040)
		kstsize = MAXKL2SIZE / (NPTEPG/SG4_LEV2SIZE);
	else
		kstsize = 1;
	kstpa = nextpa;
	nextpa += kstsize * NBPG;
	kptpa = nextpa;
	nptpages = Sysptsize;
	nextpa += nptpages * NBPG;
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
	 * Portions of the last segment of KVA space (0x3FF00000 -
	 * 0x3FFFFFFF) are mapped for a couple of purposes.  0x3FF00000
	 * for UPAGES is used for mapping the current process u-area
	 * (u + kernel stack).  The very last page (0x3FFFF000) is mapped
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
#if defined(LUNA2)
	if (mmutype == MMU_68040) {
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
		ste = &((u_int *)kstpa)[KERNELSTACK >> SG4_SHIFT1];
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
		pte = &((u_int *)kptmpa)[KERNELSTACK>>SG_ISHIFT];
		*pte = lkptpa | PG_RW | PG_CI | PG_V;
	} else
#endif
	{
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
		 * Invalidate all entries.
		 */
		epte = &((u_int *)kptmpa)[NPTEPG];
		while (pte < epte) {
			*ste++ = SG_NV;
			*pte++ = PG_NV;
		}
		/* LUNA: Uarea pt map */
		ste = (u_int *)kstpa;
		pte = (u_int *)kptmpa;
		ste[KERNELSTACK>>SG_ISHIFT] = lkptpa | SG_RW | SG_V;
		pte[KERNELSTACK>>SG_ISHIFT] = lkptpa | PG_RW | PG_CI | PG_V;
	}
	/*
	 * Invalidate all but the final entry in the last kernel PT page
	 * (u-area PTEs will be validated later).  The final entry maps
	 * the last page of physical memory.
	 */
	pte = (u_int *)lkptpa;
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
#if defined(LUNA2)
	/*
	 * Enable copy-back caching of data pages
	 */
	if (mmutype == MMU_68040)
		protopte |= PG_CCB;
#endif
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
	proc0paddr = (char *)(p0upa - firstpa);

	/*
	 * VM data structures are now initialized, set up data for
	 * the pmap module.
	 */
	avail_start = nextpa;
	avail_end = luna_ptob(maxmem)
			/* XXX allow for msgbuf */
			- luna_round_page(sizeof(struct msgbuf));
	mem_size = luna_ptob(physmem);
	virtual_avail = VM_MIN_KERNEL_ADDRESS + (nextpa - firstpa);
	virtual_end = VM_MAX_KERNEL_ADDRESS;

	/*
	 * Initialize protection array.
	 * XXX don't use a switch statement, it might produce an
	 * absolute "jmp" table.
	 */
	{
		register int *kp;

		kp = protection_codes;
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
		struct pmap *kpm = &kernel_pmap_store;

		kpm->pm_stab = Sysseg;
		kpm->pm_ptab = Sysmap;
		simple_lock_init(&kpm->pm_lock);
		kpm->pm_count = 1;
		kpm->pm_stpa = (struct ste *)kstpa;
#if defined(LUNA2)
		/*
		 * For the 040 we also initialize the free level 2
		 * descriptor mask noting that we have used:
		 *	0:		level 1 table
		 *	1 to `num':	map page tables
		 *	MAXKL2SIZE-1:	maps last-page page table
		 */
		if (mmutype == MMU_68040) {
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
#endif
	}

	/*
	 * Allocate some fixed, special purpose kernel virtual addresses
	 */
	{
		vm_offset_t va = virtual_avail;

		CADDR1 = (caddr_t)va;
		va += LUNA_PAGE_SIZE;
		CADDR2 = (caddr_t)va;
		va += LUNA_PAGE_SIZE;
		vmmap = (caddr_t)va;
		va += LUNA_PAGE_SIZE;
		ledbase = (caddr_t)va;
		va += LUNA_PAGE_SIZE;
		msgbufp = (struct msgbuf *)va;
		va += LUNA_PAGE_SIZE;
		virtual_avail = va;
	}
}
