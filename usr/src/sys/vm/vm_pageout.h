/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_pageout.h	7.3 (Berkeley) %G%
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
 *	Header file for pageout daemon.
 */

/*
 *	Exported data structures.
 */

extern int	vm_pages_needed;	/* should be some "event" structure */
simple_lock_data_t	vm_pages_needed_lock;


/*
 *	Exported routines.
 */

/*
 *	Signal pageout-daemon and wait for it.
 */

#define	VM_WAIT		{ \
			simple_lock(&vm_pages_needed_lock); \
			thread_wakeup((int)&vm_pages_needed); \
			thread_sleep((int)&vm_page_free_count, \
				&vm_pages_needed_lock, FALSE); \
			}
