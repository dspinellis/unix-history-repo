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
 *	@(#)vm_init.c	7.2 (Berkeley) %G%
 */

/*
 *	Initialize the Virtual Memory subsystem.
 */

#include "param.h"

#include "vm.h"
#include "vm_page.h"
#include "vm_kern.h"

/*
 *	vm_init initializes the virtual memory system.
 *	This is done only by the first cpu up.
 *
 *	The start and end address of physical memory is passed in.
 */

void vm_mem_init()
{
	extern vm_offset_t	avail_start, avail_end;
	extern vm_offset_t	virtual_avail, virtual_end;

	/*
	 *	Initializes resident memory structures.
	 *	From here on, all physical memory is accounted for,
	 *	and we use only virtual addresses.
	 */

	virtual_avail = vm_page_startup(avail_start, avail_end, virtual_avail);
	/*
	 * Initialize other VM packages
	 */
	vm_object_init();
	vm_map_startup();
	kmem_init(virtual_avail, virtual_end);
	pmap_init(avail_start, avail_end);
	vm_pager_init();
}
