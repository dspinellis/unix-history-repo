/* 
 * Copyright (c) 1986, Avadis Tevanian, Jr.
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
 *	@(#)vm_pageout.h	7.1 (Berkeley) %G%
 */

/*
 *	Header file for pageout daemon.
 */

#include "lock.h"

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
