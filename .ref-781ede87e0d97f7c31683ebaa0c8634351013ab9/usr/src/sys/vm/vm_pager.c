/* 
 * Copyright (c) 1985, 1986 Avadis Tevanian, Jr., Michael Wayne Young
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
 *	@(#)vm_pager.c	7.1 (Berkeley) %G%
 */

/*
 *	Paging space routine stubs.  Emulates a matchmaker-like interface
 *	for builtin pagers.
 */

#include "param.h"
#include "queue.h"
#include "malloc.h"

#include "../vm/vm_param.h"
#include "../vm/vm_pager.h"
#include "../vm/vm_page.h"
#include "../vm/vm_prot.h"
#include "../vm/vm_map.h"
#include "../vm/vm_kern.h"

#include "../vm/pmap.h"

#include "swappager.h"
#if NSWAPPAGER > 0
extern struct pagerops swappagerops;
#else
#define	swappagerops	PAGER_OPS_NULL
#endif
#include "vnodepager.h"
#if NVNODEPAGER > 0
extern struct pagerops vnodepagerops;
#else
#define	vnodepagerops	PAGER_OPS_NULL
#endif
#include "devpager.h"
#if NDEVPAGER > 0
extern struct pagerops devicepagerops;
#else
#define	devicepagerops	PAGER_OPS_NULL
#endif

struct pagerops *pagertab[] = {
	&swappagerops,		/* PG_SWAP */
	&vnodepagerops,		/* PG_VNODE */
	&devicepagerops,	/* PG_DEV */
};
int npagers = sizeof (pagertab) / sizeof (pagertab[0]);

struct pagerops *dfltpagerops = PAGER_OPS_NULL;	/* default pager */

/*
 * Kernel address space for mapping pages.
 * Used by pagers where KVAs are needed for IO.
 */
#define PAGER_MAP_SIZE	(256 * PAGE_SIZE)
vm_map_t pager_map;

void
vm_pager_init()
{
	vm_offset_t whocares1, whocares2;
	struct pagerops **pgops;

	/*
	 * Allocate a kernel submap for tracking get/put page mappings
	 */
	pager_map = kmem_suballoc(kernel_map, &whocares1, &whocares2,
				  PAGER_MAP_SIZE, FALSE);
	/*
	 * Initialize known pagers
	 */
	for (pgops = pagertab; pgops < &pagertab[npagers]; pgops++)
		(*(*pgops)->pgo_init)();
	if (dfltpagerops == PAGER_OPS_NULL)
		panic("no default pager");
}

/*
 * Allocate an instance of a pager of the given type.
 */
vm_pager_t
vm_pager_allocate(type, handle, size, prot)
	int type;
	caddr_t handle;
	vm_size_t size;
	vm_prot_t prot;
{
	vm_pager_t pager;
	struct pagerops *ops;

	ops = (type == PG_DFLT) ? dfltpagerops : pagertab[type];
	return((*ops->pgo_alloc)(handle, size, prot));
}

void
vm_pager_deallocate(pager)
	vm_pager_t	pager;
{
	if (pager == vm_pager_null)
		panic("vm_pager_deallocate: null pager");

	VM_PAGER_DEALLOC(pager);
}

vm_pager_get(pager, m, sync)
	vm_pager_t	pager;
	vm_page_t	m;
	boolean_t	sync;
{
	extern boolean_t vm_page_zero_fill();

	if (pager == vm_pager_null)
		return(vm_page_zero_fill(m) ? VM_PAGER_OK : VM_PAGER_FAIL);
	return(VM_PAGER_GET(pager, m, sync));
}

vm_pager_put(pager, m, sync)
	vm_pager_t	pager;
	vm_page_t	m;
	boolean_t	sync;
{
	if (pager == vm_pager_null)
		panic("vm_pager_put: null pager");
	return(VM_PAGER_PUT(pager, m, sync));
}

boolean_t
vm_pager_has_page(pager, offset)
	vm_pager_t	pager;
	vm_offset_t	offset;
{
	if (pager == vm_pager_null)
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
		(*(*pgops)->pgo_putpage)(VM_PAGER_NULL, VM_PAGE_NULL, FALSE);
}

vm_offset_t
vm_pager_map_page(m)
	vm_page_t	m;
{
	vm_offset_t kva;

	kva = kmem_alloc_wait(pager_map, PAGE_SIZE);
#if 1
	/*
	 * XXX: cannot use pmap_enter as the mapping would be
	 * removed by a pmap_remove_all().
	 */
	*(int *)kvtopte(kva) = VM_PAGE_TO_PHYS(m) | PG_CI | PG_V;
	TBIS(kva);
#else
	pmap_enter(vm_map_pmap(pager_map), kva, VM_PAGE_TO_PHYS(m),
		   VM_PROT_DEFAULT, TRUE);
#endif
	return(kva);
}

void
vm_pager_unmap_page(kva)
	vm_offset_t	kva;
{
#if 1
	*(int *)kvtopte(kva) = PG_NV;
	TBIS(kva);
#endif
	kmem_free_wakeup(pager_map, kva, PAGE_SIZE);
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
	return(VM_PAGER_NULL);
}

/*
 * This routine gains a reference to the object.
 * Explicit deallocation is necessary.
 */
pager_cache(object, should_cache)
	vm_object_t	object;
	boolean_t	should_cache;
{
	if (object == VM_OBJECT_NULL)
		return(KERN_INVALID_ARGUMENT);

	vm_object_cache_lock();
	vm_object_lock(object);
	object->can_persist = should_cache;
	vm_object_unlock(object);
	vm_object_cache_unlock();

	vm_object_deallocate(object);

	return(KERN_SUCCESS);
}
