/* 
 * Copyright (c) 1985, Avadis Tevanian, Jr.
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
 *	@(#)pmap.h	7.2 (Berkeley) %G%
 */

/*
 *	Machine address mapping definitions -- machine-independent
 *	section.  [For machine-dependent section, see "machine/pmap.h".]
 */

#ifndef	_PMAP_VM_
#define	_PMAP_VM_

#include <machine/pmap.h>

#ifdef KERNEL
void		pmap_bootstrap();
void		pmap_init();
void		pmap_pinit __P((struct pmap *pmap));
void		pmap_release __P((struct pmap *pmap));
vm_offset_t	pmap_map();
pmap_t		pmap_create();
pmap_t		pmap_kernel();
void		pmap_destroy();
void		pmap_reference();
void		pmap_remove();
void		pmap_remove_all();
void		pmap_copy_on_write();
void		pmap_protect();
void		pmap_enter();
vm_offset_t	pmap_extract();
void		pmap_update();
void		pmap_collect();
void		pmap_activate();
void		pmap_deactivate();
void		pmap_copy();
void		pmap_statistics();
void		pmap_clear_reference();
boolean_t	pmap_is_referenced();

void		pmap_redzone();
boolean_t	pmap_access();

extern pmap_t	kernel_pmap;
#endif

#endif	_PMAP_VM_
