/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmap.h	7.3 (Berkeley) %G%
 *
 *
 * Copyright (c) 1987, 1990 Carnegie-Mellon University.
 * All rights reserved.
 *
 * Author: Avadis Tevanian, Jr.
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
