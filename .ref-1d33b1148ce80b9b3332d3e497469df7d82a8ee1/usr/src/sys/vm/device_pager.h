/*
 * Copyright (c) 1990 University of Utah.
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)device_pager.h	8.3 (Berkeley) %G%
 */

#ifndef	_DEVICE_PAGER_
#define	_DEVICE_PAGER_	1

/*
 * Device pager private data.
 */
struct devpager {
	struct pglist	devp_pglist;	/* list of pages allocated */
	vm_object_t	devp_object;	/* object representing this device */
};
typedef struct devpager	*dev_pager_t;

#endif	/* _DEVICE_PAGER_ */
