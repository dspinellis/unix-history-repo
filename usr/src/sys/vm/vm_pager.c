/* 
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_pager.c	8.3 (Berkeley) %G%
 *
 *
 * Copyright (c) 1987, 1990 Carnegie-Mellon University.
 * All rights reserved.
 *
 * Authors: Avadis Tevanian, Jr., Michael Wayne Young
 * 
 * Permission to use, copy, modify and distribute this software and
 * its documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS" 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND 
 * FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 *
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 *
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

/*
 *	Paging space routine stubs.  Emulates a matchmaker-like interface
 *	for builtin pagers.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/malloc.h>

#include <vm/vm.h>
#include <vm/vm_page.h>
#include <vm/vm_kern.h>

#ifdef SWAPPAGER
extern struct pagerops swappagerops;
#endif

#ifdef VNODEPAGER
extern struct pagerops vnodepagerops;
#endif

#ifdef DEVPAGER
extern struct pagerops devicepagerops;
#endif

struct pagerops *pagertab[] = {
#ifdef SWAPPAGER
	&swappagerops,		/* PG_SWAP */
#else
	NULL,
#endif
#ifdef VNODEPAGER
	&vnodepagerops,		/* PG_VNODE */
#else
	NULL,
#endif
#ifdef DEVPAGER
	&devicepagerops,	/* PG_DEV */
#else
	NULL,
#endif
};
int npagers = sizeof (pagertab) / sizeof (pagertab[0]);

struct pagerops *dfltpagerops = NULL;	/* default pager */

/*
 * Kernel address space for mapping pages.
 * Used by pagers where KVAs are needed for IO.
 */
#define PAGER_MAP_SIZE	(256 * PAGE_SIZE)
vm_map_t pager_map;
vm_offset_t pager_sva, pager_eva;

void
vm_pager_init()
{
	struct pagerops **pgops;

	/*
	 * Allocate a kernel submap for tracking get/put page mappings
	 */
	pager_map = kmem_suballoc(kernel_map, &pager_sva, &pager_eva,
				  PAGER_MAP_SIZE, FALSE);
	/*
	 * Initialize known pagers
	 */
	for (pgops = pagertab; pgops < &pagertab[npagers]; pgops++)
		if (pgops)
			(*(*pgops)->pgo_init)();
	if (dfltpagerops == NULL)
		panic("no default pager");
}

/*
 * Allocate an instance of a pager of the given type.
 * Size, protection and offset parameters are passed in for pagers that
 * need to perform page-level validation (e.g. the device pager).
 */
vm_pager_t
vm_pager_allocate(type, handle, size, prot, off)
	int type;
	caddr_t handle;
	vm_size_t size;
	vm_prot_t prot;
	vm_offset_t off;
{
	vm_pager_t pager;
	struct pagerops *ops;

	ops = (type == PG_DFLT) ? dfltpagerops : pagertab[type];
	if (ops)
		return ((*ops->pgo_alloc)(handle, size, prot, off));
	return (NULL);
}

void
vm_pager_deallocate(pager)
	vm_pager_t	pager;
{
	if (pager == NULL)
		panic("vm_pager_deallocate: null pager");

	VM_PAGER_DEALLOC(pager);
}

int
vm_pager_get(pager, m, sync)
	vm_pager_t	pager;
	vm_page_t	m;
	boolean_t	sync;
{
	extern boolean_t vm_page_zero_fill();

	if (pager == NULL)
		return(vm_page_zero_fill(m) ? VM_PAGER_OK : VM_PAGER_FAIL);
	return(VM_PAGER_GET(pager, m, sync));
}

int
vm_pager_put(pager, m, sync)
	vm_pager_t	pager;
	vm_page_t	m;
	boolean_t	sync;
{
	if (pager == NULL)
		panic("vm_pager_put: null pager");
	return(VM_PAGER_PUT(pager, m, sync));
}

boolean_t
vm_pager_has_page(pager, offset)
	vm_pager_t	pager;
	vm_offset_t	offset;
{
	if (pager == NULL)
		panic("vm_pager_has_page");
	return(VM_PAGER_HASPAGE(pager, offset));
}

/*
 * Called by pageout daemon before going back to sleep.
 * Gives pagers a chance to clean up any completed async pageing operations.
 */
void
vm_pager_sync()
{
	struct pagerops **pgops;

	for (pgops = pagertab; pgops < &pagertab[npagers]; pgops++)
		if (pgops)
			(*(*pgops)->pgo_putpage)(NULL, NULL, FALSE);
}

vm_offset_t
vm_pager_map_page(m)
	vm_page_t	m;
{
	vm_offset_t kva;

#ifdef DEBUG
	if ((m->flags & PG_BUSY) == 0)
		panic("vm_pager_map_page: page not busy");
	if (m->flags & PG_PAGEROWNED)
		printf("vm_pager_map_page: page %x already in pager\n", m);
#endif
	kva = kmem_alloc_wait(pager_map, PAGE_SIZE);
#ifdef DEBUG
	m->flags |= PG_PAGEROWNED;
#endif
	pmap_enter(vm_map_pmap(pager_map), kva, VM_PAGE_TO_PHYS(m),
		   VM_PROT_DEFAULT, TRUE);
	return(kva);
}

void
vm_pager_unmap_page(kva)
	vm_offset_t	kva;
{
#ifdef DEBUG
	vm_page_t m;

	m = PHYS_TO_VM_PAGE(pmap_extract(vm_map_pmap(pager_map), kva));
#endif
	pmap_remove(vm_map_pmap(pager_map), kva, kva + PAGE_SIZE);
	kmem_free_wakeup(pager_map, kva, PAGE_SIZE);
#ifdef DEBUG
	if (m->flags & PG_PAGEROWNED)
		m->flags &= ~PG_PAGEROWNED;
	else
		printf("vm_pager_unmap_page: page %x(%x/%x) not owned\n",
		       m, kva, VM_PAGE_TO_PHYS(m));
#endif
}

vm_pager_t
vm_pager_lookup(list, handle)
	register queue_head_t *list;
	caddr_t handle;
{
	register vm_pager_t pager;

	pager = (vm_pager_t) queue_first(list);
	while (!queue_end(list, (queue_entry_t)pager)) {
		if (pager->pg_handle == handle)
			return(pager);
		pager = (vm_pager_t) queue_next(&pager->pg_list);
	}
	return(NULL);
}

/*
 * This routine gains a reference to the object.
 * Explicit deallocation is necessary.
 */
int
pager_cache(object, should_cache)
	vm_object_t	object;
	boolean_t	should_cache;
{
	if (object == NULL)
		return(KERN_INVALID_ARGUMENT);

	vm_object_cache_lock();
	vm_object_lock(object);
	if (should_cache)
		object->flags |= OBJ_CANPERSIST;
	else
		object->flags &= ~OBJ_CANPERSIST;
	vm_object_unlock(object);
	vm_object_cache_unlock();

	vm_object_deallocate(object);

	return(KERN_SUCCESS);
}
