/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_extern.h	8.2 (Berkeley) %G%
 */

struct buf;
struct loadavg;
struct proc;
struct vmspace;
struct vmtotal;
struct mount;
struct vnode;

#ifdef KGDB
void		 chgkprot __P((caddr_t, int, int));
#endif

#ifdef KERNEL
#ifdef TYPEDEF_FOR_UAP
int		 getpagesize __P((struct proc *p, void *, int *));
int		 madvise __P((struct proc *, void *, int *));
int		 mincore __P((struct proc *, void *, int *));
int		 mprotect __P((struct proc *, void *, int *));
int		 msync __P((struct proc *, void *, int *));
int		 munmap __P((struct proc *, void *, int *));
int		 obreak __P((struct proc *, void *, int *));
int		 sbrk __P((struct proc *, void *, int *));
int		 smmap __P((struct proc *, void *, int *));
int		 sstk __P((struct proc *, void *, int *));
#endif

void		 assert_wait __P((int, boolean_t));
int		 grow __P((struct proc *, u_int));
void		 iprintf __P((const char *, ...));
int		 kernacc __P((caddr_t, int, int));
int		 kinfo_loadavg __P((int, char *, int *, int, int *));
int		 kinfo_meter __P((int, caddr_t, int *, int, int *));
vm_offset_t	 kmem_alloc __P((vm_map_t, vm_size_t));
vm_offset_t	 kmem_alloc_pageable __P((vm_map_t, vm_size_t));
vm_offset_t	 kmem_alloc_wait __P((vm_map_t, vm_size_t));
void		 kmem_free __P((vm_map_t, vm_offset_t, vm_size_t));
void		 kmem_free_wakeup __P((vm_map_t, vm_offset_t, vm_size_t));
void		 kmem_init __P((vm_offset_t, vm_offset_t));
vm_offset_t	 kmem_malloc __P((vm_map_t, vm_size_t, boolean_t));
vm_map_t	 kmem_suballoc __P((vm_map_t, vm_offset_t *, vm_offset_t *,
		    vm_size_t, boolean_t));
void		 loadav __P((struct loadavg *));
void		 munmapfd __P((int));
int		 pager_cache __P((vm_object_t, boolean_t));
void		 sched __P((void));
int		 svm_allocate __P((struct proc *, void *, int *));
int		 svm_deallocate __P((struct proc *, void *, int *));
int		 svm_inherit __P((struct proc *, void *, int *));
int		 svm_protect __P((struct proc *, void *, int *));
void		 swapinit __P((void));
int		 swapon __P((struct proc *, void *, int *));
void		 swapout __P((struct proc *));
void		 swapout_threads __P((void));
int		 swfree __P((struct proc *, int));
void		 swstrategy __P((struct buf *));
void		 thread_block __P((void));
void		 thread_sleep __P((int, simple_lock_t, boolean_t));
void		 thread_wakeup __P((int));
int		 useracc __P((caddr_t, int, int));
int		 vm_allocate __P((vm_map_t,
		    vm_offset_t *, vm_size_t, boolean_t));
int		 vm_allocate_with_pager __P((vm_map_t, vm_offset_t *,
		    vm_size_t, boolean_t, vm_pager_t, vm_offset_t, boolean_t));
int		 vm_deallocate __P((vm_map_t, vm_offset_t, vm_size_t));
int		 vm_fault __P((vm_map_t, vm_offset_t, vm_prot_t, boolean_t));
void		 vm_fault_copy_entry __P((vm_map_t,
		    vm_map_t, vm_map_entry_t, vm_map_entry_t));
void		 vm_fault_unwire __P((vm_map_t, vm_offset_t, vm_offset_t));
int		 vm_fault_wire __P((vm_map_t, vm_offset_t, vm_offset_t));
int		 vm_fork __P((struct proc *, struct proc *, int));
int		 vm_inherit __P((vm_map_t,
		    vm_offset_t, vm_size_t, vm_inherit_t));
void		 vm_init_limits __P((struct proc *));
void		 vm_mem_init __P((void));
int		 vm_mmap __P((vm_map_t, vm_offset_t *, vm_size_t,
		    vm_prot_t, vm_prot_t, int, caddr_t, vm_offset_t));
int		 vm_protect __P((vm_map_t,
		    vm_offset_t, vm_size_t, boolean_t, vm_prot_t));
void		 vm_set_page_size __P((void));
void		 vmmeter __P((void));
struct vmspace	*vmspace_alloc __P((vm_offset_t, vm_offset_t, int));
struct vmspace	*vmspace_fork __P((struct vmspace *));
void		 vmspace_free __P((struct vmspace *));
void		 vmtotal __P((struct vmtotal *));
void		 vnode_pager_setsize __P((struct vnode *, u_long));
void		 vnode_pager_umount __P((struct mount *));
boolean_t	 vnode_pager_uncache __P((struct vnode *));
void		 vslock __P((caddr_t, u_int));
void		 vsunlock __P((caddr_t, u_int, int));
#endif
