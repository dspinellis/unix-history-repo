/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_kern.h	7.4 (Berkeley) %G%
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
 *	Kernel memory management definitions.
 */

void		kmem_init __P((vm_offset_t, vm_offset_t));
vm_offset_t	kmem_alloc __P((vm_map_t, vm_size_t));
vm_offset_t	kmem_alloc_pageable __P((vm_map_t, vm_size_t));
void		kmem_free __P((vm_map_t, vm_offset_t, vm_size_t));
vm_map_t	kmem_suballoc __P((vm_map_t, vm_offset_t *, vm_offset_t *,
				   vm_size_t, boolean_t));

vm_offset_t	vm_move __P((vm_map_t, vm_offset_t, vm_map_t, vm_offset_t,
			     boolean_t));

vm_offset_t	kmem_alloc_wait __P((vm_map_t, vm_size_t));
void		kmem_free_wakeup __P((vm_map_t, vm_offset_t, vm_size_t));

vm_map_t	kernel_map;
vm_map_t	mb_map;
vm_map_t	kmem_map;
vm_map_t	exec_map;
vm_map_t	phys_map;
vm_map_t	buffer_map;
