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
 *	@(#)vnode_pager.h	7.1 (Berkeley) %G%
 */

#ifndef	_VNODE_PAGER_
#define	_VNODE_PAGER_	1

/*
 * VNODE pager private data.
 */
struct vnpager {
	int		vnp_flags;	/* flags */
	struct vnode	*vnp_vp;	/* vnode */
	vm_size_t	vnp_size;	/* vnode current size */
};
typedef struct vnpager	*vn_pager_t;

#define VN_PAGER_NULL	((vn_pager_t)0)

#define	VNP_PAGING	0x01		/* vnode used for pageout */
#define VNP_CACHED	0x02		/* vnode is cached */

#ifdef KERNEL

void		vnode_pager_init();
vm_pager_t	vnode_pager_alloc();
void		vnode_pager_dealloc();
int		vnode_pager_getpage(), vnode_pager_putpage();
boolean_t	vnode_pager_haspage();

struct pagerops vnodepagerops = {
	vnode_pager_init,
	vnode_pager_alloc,
	vnode_pager_dealloc,
	vnode_pager_getpage,
	vnode_pager_putpage,
	vnode_pager_haspage
};

#endif

#endif	/* _VNODE_PAGER_ */
