/*
 * Copyright (c) 1990 University of Utah.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)device_pager.h	7.1 (Berkeley) %G%
 */

#ifndef	_DEVICE_PAGER_
#define	_DEVICE_PAGER_	1

/*
 * Device pager private data.
 */
struct devpager {
	queue_head_t	devp_list;	/* list of managed devices */
	dev_t		devp_dev;	/* devno of device */
	vm_page_t	devp_pages;	/* page structs for device */
	int		devp_npages;	/* size of device in pages */
	int		devp_count;	/* reference count */
	vm_object_t	devp_object;	/* object representing this device */
};
typedef struct devpager	*dev_pager_t;

#define DEV_PAGER_NULL	((dev_pager_t)0)

#ifdef KERNEL

void		dev_pager_init();
vm_pager_t	dev_pager_alloc();
void		dev_pager_dealloc();
boolean_t	dev_pager_getpage(), dev_pager_putpage();
boolean_t	dev_pager_haspage();

struct pagerops devicepagerops = {
	dev_pager_init,
	dev_pager_alloc,
	dev_pager_dealloc,
	dev_pager_getpage,
	dev_pager_putpage,
	dev_pager_haspage
};

#endif

#endif	/* _DEVICE_PAGER_ */
