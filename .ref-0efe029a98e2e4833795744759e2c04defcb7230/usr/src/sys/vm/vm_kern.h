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
 *	@(#)vm_kern.h	7.1 (Berkeley) %G%
 */

/*
 *	Kernel memory management definitions.
 */

void		kmem_init();
vm_offset_t	kmem_alloc();
vm_offset_t	kmem_alloc_pageable();
void		kmem_free();
vm_map_t	kmem_suballoc();

vm_offset_t	vm_move();

vm_offset_t	kmem_alloc_wait();
void		kmem_free_wakeup();

vm_map_t	kernel_map;
vm_map_t	mb_map;
vm_map_t	kmem_map;
vm_map_t	exec_map;
vm_map_t	phys_map;
vm_map_t	buffer_map;
