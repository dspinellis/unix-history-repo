/* 
 * Copyright (c) 1985, Avadis Tevanian, Jr., Michael Wayne Young
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
 *	@(#)vm_object.h	7.1 (Berkeley) %G%
 */

/*
 *	Virtual memory object module definitions.
 */

#ifndef	_VM_OBJECT_
#define	_VM_OBJECT_

#ifdef KERNEL
#include "types.h"
#include "lock.h"
#include "queue.h"
#include "../vm/vm_pager.h"
#else
#include <sys/types.h>
#include <sys/lock.h>
#include <sys/queue.h>
#include <vm/vm_pager.h>
#endif

/*
 *	Types defined:
 *
 *	vm_object_t		Virtual memory object.
 */

struct vm_object {
	queue_chain_t		memq;		/* Resident memory */
	queue_chain_t		object_list;	/* list of all objects */
	simple_lock_data_t	Lock;		/* Synchronization */
	int			LockHolder;
	int			ref_count;	/* How many refs?? */
	vm_size_t		size;		/* Object size */
	int			resident_page_count;
						/* number of resident pages */
	struct vm_object	*copy;		/* Object that holds copies of
						   my changed pages */
	vm_pager_t		pager;		/* Where to get data */
	boolean_t		pager_ready;	/* Have pager fields been filled? */
	vm_offset_t		paging_offset;	/* Offset into paging space */
	struct vm_object	*shadow;	/* My shadow */
	vm_offset_t		shadow_offset;	/* Offset in shadow */
	unsigned int
				paging_in_progress:16,
						/* Paging (in or out) - don't
						   collapse or destroy */
	/* boolean_t */		can_persist:1,	/* allow to persist */
	/* boolean_t */		internal:1;	/* internally created object */
	queue_chain_t		cached_list;	/* for persistence */
};

typedef struct vm_object	*vm_object_t;

struct vm_object_hash_entry {
	queue_chain_t		hash_links;	/* hash chain links */
	vm_object_t		object;		/* object we represent */
};

typedef struct vm_object_hash_entry	*vm_object_hash_entry_t;

#ifdef	KERNEL
queue_head_t	vm_object_cached_list;	/* list of objects persisting */
int		vm_object_cached;	/* size of cached list */
simple_lock_data_t	vm_cache_lock;	/* lock for object cache */

queue_head_t	vm_object_list;		/* list of allocated objects */
long		vm_object_count;	/* count of all objects */
simple_lock_data_t	vm_object_list_lock;
					/* lock for object list and count */

vm_object_t	kernel_object;		/* the single kernel object */
vm_object_t	kmem_object;

#define	vm_object_cache_lock()		simple_lock(&vm_cache_lock)
#define	vm_object_cache_unlock()	simple_unlock(&vm_cache_lock)
#endif	KERNEL

#define	VM_OBJECT_NULL		((vm_object_t) 0)

/*
 *	Declare procedures that operate on VM objects.
 */

void		vm_object_init ();
void		vm_object_terminate();
vm_object_t	vm_object_allocate();
void		vm_object_reference();
void		vm_object_deallocate();
void		vm_object_pmap_copy();
void		vm_object_pmap_remove();
void		vm_object_page_remove();
void		vm_object_shadow();
void		vm_object_copy();
void		vm_object_collapse();
vm_object_t	vm_object_lookup();
void		vm_object_enter();
void		vm_object_setpager();
#define		vm_object_cache(pager)		pager_cache(vm_object_lookup(pager),TRUE)
#define		vm_object_uncache(pager)	pager_cache(vm_object_lookup(pager),FALSE)

void		vm_object_cache_clear();
void		vm_object_print();

#if	VM_OBJECT_DEBUG
#define	vm_object_lock_init(object)	{ simple_lock_init(&(object)->Lock); (object)->LockHolder = 0; }
#define	vm_object_lock(object)		{ simple_lock(&(object)->Lock); (object)->LockHolder = (int) current_thread(); }
#define	vm_object_unlock(object)	{ (object)->LockHolder = 0; simple_unlock(&(object)->Lock); }
#define	vm_object_lock_try(object)	(simple_lock_try(&(object)->Lock) ? ( ((object)->LockHolder = (int) current_thread()) , TRUE) : FALSE)
#define	vm_object_sleep(event, object, interruptible) \
					{ (object)->LockHolder = 0; thread_sleep((event), &(object)->Lock, (interruptible)); }
#else	VM_OBJECT_DEBUG
#define	vm_object_lock_init(object)	simple_lock_init(&(object)->Lock)
#define	vm_object_lock(object)		simple_lock(&(object)->Lock)
#define	vm_object_unlock(object)	simple_unlock(&(object)->Lock)
#define	vm_object_lock_try(object)	simple_lock_try(&(object)->Lock)
#define	vm_object_sleep(event, object, interruptible) \
					thread_sleep((event), &(object)->Lock, (interruptible))
#endif	VM_OBJECT_DEBUG

#endif	_VM_OBJECT_
