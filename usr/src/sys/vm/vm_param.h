/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_param.h	7.2 (Berkeley) %G%
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
 *	Machine independent virtual memory parameters.
 */

#ifndef	_VM_PARAM_
#define	_VM_PARAM_

#ifdef KERNEL
#include "machine/vmparam.h"
#else
#include <machine/vmparam.h>
#endif

/*
 * This belongs in types.h, but breaks too many existing programs.
 */
typedef int	boolean_t;
#define	TRUE	1
#define	FALSE	0

/*
 *	The machine independent pages are refered to as PAGES.  A page
 *	is some number of hardware pages, depending on the target machine.
 */

/*
 *	All references to the size of a page should be done with PAGE_SIZE
 *	or PAGE_SHIFT.  The fact they are variables is hidden here so that
 *	we can easily make them constant if we so desire.
 */

#define	PAGE_SIZE	page_size	/* size of page in addressible units */
#define PAGE_SHIFT	page_shift	/* number of bits to shift for pages */

/* 
 *	Return values from the VM routines.
 */
#define	KERN_SUCCESS		0
#define	KERN_INVALID_ADDRESS	1
#define	KERN_PROTECTION_FAILURE	2
#define	KERN_NO_SPACE		3
#define	KERN_INVALID_ARGUMENT	4
#define	KERN_FAILURE		5
#define	KERN_RESOURCE_SHORTAGE	6
#define	KERN_NOT_RECEIVER	7
#define	KERN_NO_ACCESS		8

#ifdef	ASSEMBLER
#else	ASSEMBLER
/*
 *	Convert addresses to pages and vice versa.
 *	No rounding is used.
 */

#ifdef	KERNEL
#define	atop(x)		(((unsigned)(x)) >> page_shift)
#define	ptoa(x)		((vm_offset_t)((x) << page_shift))
#endif	KERNEL

/*
 *	Round off or truncate to the nearest page.  These will work
 *	for either addresses or counts.  (i.e. 1 byte rounds to 1 page
 *	bytes.
 */

#ifdef	KERNEL
#define round_page(x)	((vm_offset_t)((((vm_offset_t)(x)) + page_mask) & ~page_mask))
#define trunc_page(x)	((vm_offset_t)(((vm_offset_t)(x)) & ~page_mask))
#else	KERNEL
#define	round_page(x)	((((vm_offset_t)(x) + (vm_page_size - 1)) / vm_page_size) * vm_page_size)
#define	trunc_page(x)	((((vm_offset_t)(x)) / vm_page_size) * vm_page_size)
#endif	KERNEL

#ifdef	KERNEL
extern vm_size_t	page_size;	/* machine independent page size */
extern vm_size_t	page_mask;	/* page_size - 1; mask for
						   offset within page */
extern int		page_shift;	/* shift to use for page size */

extern vm_size_t	mem_size;	/* size of physical memory (bytes) */
extern vm_offset_t	first_addr;	/* first physical page */
extern vm_offset_t	last_addr;	/* last physical page */
#endif	KERNEL

#endif	ASSEMBLER

#endif	_VM_PARAM_
