/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmap.h	7.10 (Berkeley) %G%
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

/*
 * Each machine dependent implementation is expected to
 * keep certain statistics.  They may do this anyway they
 * so choose, but are expected to return the statistics
 * in the following structure.
 */
struct pmap_statistics {
	long		resident_count;	/* # of pages mapped (total)*/
	long		wired_count;	/* # of pages wired */
};
typedef struct pmap_statistics	*pmap_statistics_t;

#include <machine/pmap.h>

#ifdef KERNEL
__BEGIN_DECLS
void		*pmap_bootstrap_alloc __P((int));
void		 pmap_bootstrap( /* machine dependent */ );
void		 pmap_change_wiring __P((pmap_t, vm_offset_t, boolean_t));
void		 pmap_clear_modify __P((vm_offset_t pa));
void		 pmap_clear_reference __P((vm_offset_t pa));
void		 pmap_collect __P((pmap_t));
void		 pmap_copy __P((pmap_t,
		    pmap_t, vm_offset_t, vm_size_t, vm_offset_t));
void		 pmap_copy_page __P((vm_offset_t, vm_offset_t));
pmap_t		 pmap_create __P((vm_size_t));
void		 pmap_destroy __P((pmap_t));
void		 pmap_enter __P((pmap_t,
		    vm_offset_t, vm_offset_t, vm_prot_t, boolean_t));
vm_offset_t	 pmap_extract __P((pmap_t, vm_offset_t));
void		 pmap_init __P((vm_offset_t, vm_offset_t));
boolean_t	 pmap_is_modified __P((vm_offset_t pa));
boolean_t	 pmap_is_referenced __P((vm_offset_t pa));
vm_offset_t	 pmap_map __P((vm_offset_t, vm_offset_t, vm_offset_t, int));
void		 pmap_page_protect __P((vm_offset_t, vm_prot_t));
void		 pmap_pageable __P((pmap_t,
		    vm_offset_t, vm_offset_t, boolean_t));
vm_offset_t	 pmap_phys_address __P((int));
void		 pmap_pinit __P((pmap_t));
void		 pmap_protect __P((pmap_t,
		    vm_offset_t, vm_offset_t, vm_prot_t));
void		 pmap_reference __P((pmap_t));
void		 pmap_release __P((pmap_t));
void		 pmap_remove __P((pmap_t, vm_offset_t, vm_offset_t));
void		 pmap_update __P((void));
void		 pmap_zero_page __P((vm_offset_t));
__END_DECLS
#endif

#endif /* _PMAP_VM_ */
