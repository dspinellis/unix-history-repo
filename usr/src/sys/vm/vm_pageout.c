/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_pageout.c	7.9 (Berkeley) %G%
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
 *	The proverbial page-out daemon.
 */

#include <sys/param.h>

#include <vm/vm.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>

int	vm_pages_needed;	/* Event on which pageout daemon sleeps */

int	vm_page_free_min_sanity = 40;

/*
 *	vm_pageout_scan does the dirty work for the pageout daemon.
 */
void
vm_pageout_scan()
{
	register vm_page_t	m;
	register int		page_shortage;
	register int		s;
	register int		pages_freed;
	int			free;

	/*
	 *	Only continue when we want more pages to be "free"
	 */

	s = splimp();
	simple_lock(&vm_page_queue_free_lock);
	free = cnt.v_free_count;
	simple_unlock(&vm_page_queue_free_lock);
	splx(s);

	if (free < cnt.v_free_target) {
		swapout_threads();

		/*
		 *	Be sure the pmap system is updated so
		 *	we can scan the inactive queue.
		 */

		pmap_update();
	}

	/*
	 *	Acquire the resident page system lock,
	 *	as we may be changing what's resident quite a bit.
	 */
	vm_page_lock_queues();

	/*
	 *	Start scanning the inactive queue for pages we can free.
	 *	We keep scanning until we have enough free pages or
	 *	we have scanned through the entire queue.  If we
	 *	encounter dirty pages, we start cleaning them.
	 */

	pages_freed = 0;
	m = (vm_page_t) queue_first(&vm_page_queue_inactive);
	while (!queue_end(&vm_page_queue_inactive, (queue_entry_t) m)) {
		vm_page_t	next;

		s = splimp();
		simple_lock(&vm_page_queue_free_lock);
		free = cnt.v_free_count;
		simple_unlock(&vm_page_queue_free_lock);
		splx(s);

		if (free >= cnt.v_free_target)
			break;

		if (m->flags & PG_CLEAN) {
			next = (vm_page_t) queue_next(&m->pageq);
			if (pmap_is_referenced(VM_PAGE_TO_PHYS(m))) {
				vm_page_activate(m);
				cnt.v_reactivated++;
			}
			else {
				register vm_object_t	object;
				object = m->object;
				if (!vm_object_lock_try(object)) {
					/*
					 *	Can't lock object -
					 *	skip page.
					 */
					m = next;
					continue;
				}
				pmap_page_protect(VM_PAGE_TO_PHYS(m),
						  VM_PROT_NONE);
				vm_page_free(m);	/* will dequeue */
				pages_freed++;
				vm_object_unlock(object);
			}
			m = next;
		}
		else {
			/*
			 *	If a page is dirty, then it is either
			 *	being washed (but not yet cleaned)
			 *	or it is still in the laundry.  If it is
			 *	still in the laundry, then we start the
			 *	cleaning operation.
			 */

			if (m->flags & PG_LAUNDRY) {
				/*
				 *	Clean the page and remove it from the
				 *	laundry.
				 *
				 *	We set the busy bit to cause
				 *	potential page faults on this page to
				 *	block.
				 *
				 *	And we set pageout-in-progress to keep
				 *	the object from disappearing during
				 *	pageout.  This guarantees that the
				 *	page won't move from the inactive
				 *	queue.  (However, any other page on
				 *	the inactive queue may move!)
				 */

				register vm_object_t	object;
				register vm_pager_t	pager;
				int			pageout_status;

				object = m->object;
				if (!vm_object_lock_try(object)) {
					/*
					 *	Skip page if we can't lock
					 *	its object
					 */
					m = (vm_page_t) queue_next(&m->pageq);
					continue;
				}

				pmap_page_protect(VM_PAGE_TO_PHYS(m),
						  VM_PROT_NONE);
				m->flags |= PG_BUSY;
				cnt.v_pageouts++;

				/*
				 *	Try to collapse the object before
				 *	making a pager for it.  We must
				 *	unlock the page queues first.
				 */
				vm_page_unlock_queues();

				vm_object_collapse(object);

				object->paging_in_progress++;
				vm_object_unlock(object);

				/*
				 *	Do a wakeup here in case the following
				 *	operations block.
				 */
				thread_wakeup((int) &cnt.v_free_count);

				/*
				 *	If there is no pager for the page,
				 *	use the default pager.  If there's
				 *	no place to put the page at the
				 *	moment, leave it in the laundry and
				 *	hope that there will be paging space
				 *	later.
				 */

				if ((pager = object->pager) == NULL) {
					pager = vm_pager_allocate(PG_DFLT,
								  (caddr_t)0,
								  object->size,
								  VM_PROT_ALL);
					if (pager != NULL) {
						vm_object_setpager(object,
							pager, 0, FALSE);
					}
				}
				pageout_status = pager ?
					vm_pager_put(pager, m, FALSE) :
					VM_PAGER_FAIL;
				vm_object_lock(object);
				vm_page_lock_queues();
				next = (vm_page_t) queue_next(&m->pageq);

				switch (pageout_status) {
				case VM_PAGER_OK:
				case VM_PAGER_PEND:
					m->flags &= ~PG_LAUNDRY;
					break;
				case VM_PAGER_BAD:
					/*
					 * Page outside of range of object.
					 * Right now we essentially lose the
					 * changes by pretending it worked.
					 * XXX dubious, what should we do?
					 */
					m->flags &= ~PG_LAUNDRY;
					m->flags |= PG_CLEAN;
					pmap_clear_modify(VM_PAGE_TO_PHYS(m));
					break;
				case VM_PAGER_FAIL:
				case VM_PAGER_ERROR:
					/*
					 * If page couldn't be paged out, then
					 * reactivate the page so it doesn't
					 * clog the inactive list.  (We will
					 * try paging out it again later).
					 */
					vm_page_activate(m);
					break;
				}

				pmap_clear_reference(VM_PAGE_TO_PHYS(m));

				/*
				 * If the operation is still going, leave
				 * the page busy to block all other accesses.
				 * Also, leave the paging in progress
				 * indicator set so that we don't attempt an
				 * object collapse.
				 */
				if (pageout_status != VM_PAGER_PEND) {
					m->flags &= ~PG_BUSY;
					PAGE_WAKEUP(m);
					object->paging_in_progress--;
				}
				thread_wakeup((int) object);
				vm_object_unlock(object);
				m = next;
			}
			else
				m = (vm_page_t) queue_next(&m->pageq);
		}
	}
	
	/*
	 *	Compute the page shortage.  If we are still very low on memory
	 *	be sure that we will move a minimal amount of pages from active
	 *	to inactive.
	 */

	page_shortage = cnt.v_inactive_target - cnt.v_inactive_count;
	page_shortage -= cnt.v_free_count;

	if ((page_shortage <= 0) && (pages_freed == 0))
		page_shortage = 1;

	while (page_shortage > 0) {
		/*
		 *	Move some more pages from active to inactive.
		 */

		if (queue_empty(&vm_page_queue_active)) {
			break;
		}
		m = (vm_page_t) queue_first(&vm_page_queue_active);
		vm_page_deactivate(m);
		page_shortage--;
	}

	vm_page_unlock_queues();
}

/*
 *	vm_pageout is the high level pageout daemon.
 */

void vm_pageout()
{
	(void) spl0();

	/*
	 *	Initialize some paging parameters.
	 */

	if (cnt.v_free_min == 0) {
		cnt.v_free_min = cnt.v_free_count / 20;
		if (cnt.v_free_min < 3)
			cnt.v_free_min = 3;

		if (cnt.v_free_min > vm_page_free_min_sanity)
			cnt.v_free_min = vm_page_free_min_sanity;
	}

	if (cnt.v_free_target == 0)
		cnt.v_free_target = (cnt.v_free_min * 4) / 3;

	if (cnt.v_inactive_target == 0)
		cnt.v_inactive_target = cnt.v_free_min * 2;

	if (cnt.v_free_target <= cnt.v_free_min)
		cnt.v_free_target = cnt.v_free_min + 1;

	if (cnt.v_inactive_target <= cnt.v_free_target)
		cnt.v_inactive_target = cnt.v_free_target + 1;

	/*
	 *	The pageout daemon is never done, so loop
	 *	forever.
	 */

	simple_lock(&vm_pages_needed_lock);
	while (TRUE) {
		thread_sleep((int) &vm_pages_needed, &vm_pages_needed_lock,
			     FALSE);
		vm_pageout_scan();
		vm_pager_sync();
		simple_lock(&vm_pages_needed_lock);
		thread_wakeup((int) &cnt.v_free_count);
	}
}
