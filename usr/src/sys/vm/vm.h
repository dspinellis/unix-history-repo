/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm.h	7.1 (Berkeley) %G%
 */

#ifndef VM_H
#define VM_H
#include <vm/vm_param.h>
#include <vm/lock.h>
#include <vm/queue.h>
#include <vm/vm_prot.h>
#include <vm/vm_inherit.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_statistics.h>
#include <vm/pmap.h>

/*
 * Shareable process virtual address space.
 * May eventually be merged with vm_map.
 * Several fields are temporary (text, data stuff).
 */
struct vmspace {
	struct	vm_map vm_map;	/* VM address map */
	struct	pmap vm_pmap;	/* private physical map */
	int	vm_refcnt;	/* number of references */
	caddr_t	vm_shm;		/* SYS5 shared memory private data XXX */
/* we copy from vm_startcopy to the end of the structure on fork */
#define vm_startcopy vm_rssize
	segsz_t vm_rssize; 	/* current resident set size in pages */
	segsz_t vm_swrss;	/* resident set size before last swap */
	segsz_t vm_tsize;	/* text size (pages) XXX */
	segsz_t vm_dsize;	/* data size (pages) XXX */
	segsz_t vm_ssize;	/* stack size (pages) */
	caddr_t	vm_taddr;	/* user virtual address of text XXX */
	caddr_t	vm_daddr;	/* user virtual address of data XXX */
	caddr_t vm_maxsaddr;	/* user VA at max stack growth */
};

struct	vmspace *vmspace_alloc __P((vm_offset_t min, vm_offset_t max,
			int pageable));
struct	vmspace *vmspace_fork __P((struct vmspace *));
void	vmspace_free __P((struct vmspace *));
#endif /* VM_H */
