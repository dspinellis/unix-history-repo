/*
 * Copyright (c) 1987, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_malloc.c	7.37 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/map.h>
#include <sys/kernel.h>
#include <sys/malloc.h>

#include <vm/vm.h>
#include <vm/vm_kern.h>

struct kmembuckets bucket[MINBUCKET + 16];
struct kmemstats kmemstats[M_LAST];
struct kmemusage *kmemusage;
char *memname[] = INITKMEMNAMES;
char *kmembase, *kmemlimit;
char *memname[] = INITKMEMNAMES;
long malloc_reentered;
#define IN { if (malloc_reentered) panic("malloc reentered");\
			else malloc_reentered = 1;}
#define OUT (malloc_reentered = 0)

#ifdef DIAGNOSTIC
/*
 * This structure provides a set of masks to catch unaligned frees.
 */
long addrmask[] = { 0,
	0x00000001, 0x00000003, 0x00000007, 0x0000000f,
	0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
	0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
	0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
};

/*
 * The WEIRD_ADDR is used as known text to copy into free objects so
 * that modifications after frees can be detected.
 */
#define WEIRD_ADDR	0xdeadbeef
#define MAX_COPY	32

/*
 * Normally the first word of the structure is used to hold the list
 * pointer for free objects. However, when running with diagnostics,
 * we use the third and fourth fields, so as to catch modifications
 * in the most commonly trashed first two words.
 */
struct freelist {
	long	spare0;
	short	type;
	long	spare1;
	caddr_t	next;
};
#else /* !DIAGNOSTIC */
struct freelist {
	caddr_t	next;
};
#endif /* DIAGNOSTIC */

struct uselist {
	struct	uselist *next;
	caddr_t	mem;
	long	size;
	long	type;
} *listhd;

/*
 * Allocate a block of memory
 */
void *
malloc(size, type, flags)
	unsigned long size;
	int type, flags;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	register struct freelist *freep;
	long indx, npg, alloc, allocsize;
	int s;
	caddr_t va, cp, rp;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];

#ifdef DIAGNOSTIC
	if (((unsigned long)type) > M_LAST)
		panic("malloc - bogus type");
	if (type == M_NAMEI)
		curproc->p_spare[0]++;
	indx = BUCKETINDX(size);
	kbp = &bucket[indx];
	s = splimp();
	IN;
#ifdef KMEMSTATS
	while (ksp->ks_memuse >= ksp->ks_limit) {
		if (flags & M_NOWAIT) {
			OUT;
			splx(s);
			return ((void *) NULL);
		}
		if (ksp->ks_limblocks < 65535)
			ksp->ks_limblocks++;
		OUT;
		tsleep((caddr_t)ksp, PSWP+2, memname[type], 0);
		IN;
	}
	ksp->ks_size |= 1 << indx;
#endif
#ifdef DIAGNOSTIC
	copysize = 1 << indx < MAX_COPY ? 1 << indx : MAX_COPY;
#endif
	if (kbp->kb_next == NULL) {
		kbp->kb_last = NULL;
		if (size > MAXALLOCSAVE)
			allocsize = roundup(size, CLBYTES);
		else
			allocsize = 1 << indx;
		npg = clrnd(btoc(allocsize));
		va = (caddr_t) kmem_malloc(kmem_map, (vm_size_t)ctob(npg),
					   !(flags & M_NOWAIT));
		if (va == NULL) {
			OUT;
			splx(s);
			return ((void *) NULL);
		}
#ifdef KMEMSTATS
		kbp->kb_total += kbp->kb_elmpercl;
#endif
		kup = btokup(va);
		kup->ku_indx = indx;
		if (allocsize > MAXALLOCSAVE) {
			if (npg > 65535)
				panic("malloc: allocation too large");
			kup->ku_pagecnt = npg;
#ifdef KMEMSTATS
			ksp->ks_memuse += allocsize;
#endif
			goto out;
		}
#ifdef KMEMSTATS
		kup->ku_freecnt = kbp->kb_elmpercl;
		kbp->kb_totalfree += kbp->kb_elmpercl;
#endif
		/*
		 * Just in case we blocked while allocating memory,
		 * and someone else also allocated memory for this
		 * bucket, don't assume the list is still empty.
		 */
		savedlist = kbp->kb_next;
		rp = kbp->kb_next; /* returned while blocked in vmemall */
		for (cp = kbp->kb_next; cp >= va; cp -= allocsize) {
			((caddr_t *)cp)[2] = (cp > va ? cp - allocsize : rp);
			if (indx == 7) {
				long *lp = (long *)cp;
				lp[0] = lp[1] = lp[3] = lp[4] = -1;
			}
		}
	}
	va = kbp->kb_next;
	kbp->kb_next = ((caddr_t *)va)[2];
	if (indx == 7) {
		long *lp = (long *)va;
		if (lp[0] != -1 || lp[1] != -1 || lp[3] != -1 || lp[4] != -1)
			panic("malloc meddled");
	}
#ifdef KMEMSTATS
	kup = btokup(va);
	if (kup->ku_indx != indx)
		panic("malloc: wrong bucket");
	if (kup->ku_freecnt == 0)
		panic("malloc: lost data");
	kup->ku_freecnt--;
	kbp->kb_totalfree--;
	ksp->ks_memuse += 1 << indx;
out:
	kbp->kb_calls++;
	ksp->ks_inuse++;
	ksp->ks_calls++;
	if (ksp->ks_memuse > ksp->ks_maxused)
		ksp->ks_maxused = ksp->ks_memuse;
#else
out:
#endif
	if (size > 64 && size <= 128) {
		mlp = (struct uselist *)malloc(sizeof(*mlp), M_TEMP, M_WAITOK);
		mlp->type = type;
		mlp->size = size;
		mlp->mem = va;
		mlp->next = listhd;
		listhd = mlp;
	}
	OUT;
	splx(s);
	return ((void *) va);
}

#ifdef DIAGNOSTIC
long addrmask[] = { 0x00000000,
	0x00000001, 0x00000003, 0x00000007, 0x0000000f,
	0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
	0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
	0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
};
#endif /* DIAGNOSTIC */

/*
 * Free a block of memory allocated by malloc.
 */
void
free(addr, type)
	void *addr;
	int type;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	register struct freelist *freep;
	long size;
	int s;
#ifdef DIAGNOSTIC
	caddr_t cp;
	long *end, *lp, alloc, copysize;
#endif
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];
#endif

	kup = btokup(addr);
	size = 1 << kup->ku_indx;
#ifdef DIAGNOSTIC
	if (size > NBPG * CLSIZE)
		alloc = addrmask[BUCKETINDX(NBPG * CLSIZE)];
	else
		alloc = addrmask[kup->ku_indx];
	if (((u_long)addr & alloc) != 0) {
		printf("free: unaligned addr 0x%x, size %d, type %d, mask %d\n",
			addr, size, type, alloc);
		panic("free: unaligned addr");
	}
#endif /* DIAGNOSTIC */
	size = 1 << kup->ku_indx;
	kbp = &bucket[kup->ku_indx];
	s = splimp();
	if (size == 128) {
		struct uselist *mlp, *pmlp;

		mlp = listhd;
		if (mlp->mem == addr)
			listhd = mlp->next;
		else for (pmlp = mlp, mlp = mlp->next ; mlp; mlp = mlp->next) {
			if (mlp->mem == addr) {
				pmlp->next = mlp->next;
				break;
			}
			pmlp = mlp;
		}
		if (mlp == NULL)
			printf("free: lost type %s size %d\n", memname[type],
			    size);
		else
			free(mlp, M_TEMP);
	}
#ifdef DIAGNOSTIC
	/*
	 * Check for returns of data that do not point to the
	 * beginning of the allocation.
	 */
	if (type == M_NAMEI)
		curproc->p_spare[0]--;
	if (size > NBPG * CLSIZE)
		alloc = addrmask[BUCKETINDX(NBPG * CLSIZE)];
	else
		alloc = addrmask[kup->ku_indx];
	if (((u_long)addr & alloc) != 0)
		panic("free: unaligned addr 0x%x, size %d, type %s, mask %d\n",
			addr, size, memname[type], alloc);
#endif /* DIAGNOSTIC */
	IN;
	if (size > MAXALLOCSAVE) {
		kmem_free(kmem_map, (vm_offset_t)addr, ctob(kup->ku_pagecnt));
#ifdef KMEMSTATS
		size = kup->ku_pagecnt << PGSHIFT;
		ksp->ks_memuse -= size;
		kup->ku_indx = 0;
		kup->ku_pagecnt = 0;
		if (ksp->ks_memuse + size >= ksp->ks_limit &&
		    ksp->ks_memuse < ksp->ks_limit)
			wakeup((caddr_t)ksp);
		ksp->ks_inuse--;
		kbp->kb_total -= 1;
#endif
		splx(s);
		return;
	}
	freep = (struct freelist *)addr;
#ifdef DIAGNOSTIC
	/*
	 * Check for multiple frees. Use a quick check to see if
	 * it looks free before laboriously searching the freelist.
	 */
	if (freep->spare0 == WEIRD_ADDR) {
		for (cp = kbp->kb_next; cp; cp = *(caddr_t *)cp) {
			if (addr != cp)
				continue;
			printf("multiply freed item 0x%x\n", addr);
			panic("free: duplicated free");
		}
	}
	/*
	 * Copy in known text to detect modification after freeing
	 * and to make it look free. Also, save the type being freed
	 * so we can list likely culprit if modification is detected
	 * when the object is reallocated.
	 */
	copysize = size < MAX_COPY ? size : MAX_COPY;
	end = (long *)&((caddr_t)addr)[copysize];
	for (lp = (long *)addr; lp < end; lp++)
		*lp = WEIRD_ADDR;
	freep->type = type;
#endif /* DIAGNOSTIC */
	if (size == 128) {
		long *lp = (long *)addr;
		lp[0] = lp[1] = lp[3] = lp[4] = -1;
	}
#ifdef KMEMSTATS
	kup->ku_freecnt++;
	if (kup->ku_freecnt >= kbp->kb_elmpercl)
		if (kup->ku_freecnt > kbp->kb_elmpercl)
			panic("free: multiple frees");
		else if (kbp->kb_totalfree > kbp->kb_highwat)
			kbp->kb_couldfree++;
	kbp->kb_totalfree++;
	ksp->ks_memuse -= size;
	if (ksp->ks_memuse + size >= ksp->ks_limit &&
	    ksp->ks_memuse < ksp->ks_limit)
		wakeup((caddr_t)ksp);
	ksp->ks_inuse--;
#endif
	if (kbp->kb_next == NULL)
		kbp->kb_next = addr;
	else
		((struct freelist *)kbp->kb_last)->next = addr;
	freep->next = NULL;
	kbp->kb_last = addr;
	OUT;
	splx(s);
}

/*
 * Initialize the kernel memory allocator
 */
kmeminit()
{
	register long indx;
	int npg;

#if	((MAXALLOCSAVE & (MAXALLOCSAVE - 1)) != 0)
		ERROR!_kmeminit:_MAXALLOCSAVE_not_power_of_2
#endif
#if	(MAXALLOCSAVE > MINALLOCSIZE * 32768)
		ERROR!_kmeminit:_MAXALLOCSAVE_too_big
#endif
#if	(MAXALLOCSAVE < CLBYTES)
		ERROR!_kmeminit:_MAXALLOCSAVE_too_small
#endif
	npg = VM_KMEM_SIZE/ NBPG;
	kmemusage = (struct kmemusage *) kmem_alloc(kernel_map,
		(vm_size_t)(npg * sizeof(struct kmemusage)));
	kmem_map = kmem_suballoc(kernel_map, (vm_offset_t *)&kmembase,
		(vm_offset_t *)&kmemlimit, (vm_size_t)(npg * NBPG), FALSE);
#ifdef KMEMSTATS
	for (indx = 0; indx < MINBUCKET + 16; indx++) {
		if (1 << indx >= CLBYTES)
			bucket[indx].kb_elmpercl = 1;
		else
			bucket[indx].kb_elmpercl = CLBYTES / (1 << indx);
		bucket[indx].kb_highwat = 5 * bucket[indx].kb_elmpercl;
	}
	for (indx = 0; indx < M_LAST; indx++)
		kmemstats[indx].ks_limit = npg * NBPG * 6 / 10;
#endif
}
