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
 *	@(#)vm_pager.h	7.2 (Berkeley) %G%
 */

/*
 * Pager routine interface definition.
 * For BSD we use a cleaner version of the internal pager interface.
 */

#ifndef	_VM_PAGER_
#define	_VM_PAGER_

struct	pager_struct {
	queue_head_t	pg_list;	/* links for list management */
	caddr_t		pg_handle;	/* external handle (vp, dev, fp) */
	int		pg_type;	/* type of pager */
	struct pagerops	*pg_ops;	/* pager operations */
	caddr_t		pg_data;	/* private pager data */
};
typedef	struct pager_struct *vm_pager_t;

/* pager types */
#define PG_DFLT		-1
#define	PG_SWAP		0
#define	PG_VNODE	1
#define PG_DEVICE	2

struct	pagerops {
	void		(*pgo_init)();		/* initialize pager */
	vm_pager_t	(*pgo_alloc)();		/* allocate pager */
	void		(*pgo_dealloc)();	/* disassociate */
	int		(*pgo_getpage)();	/* get (read) page */
	int		(*pgo_putpage)();	/* put (write) page */
	boolean_t  	(*pgo_haspage)();	/* does pager have page? */
};

/*
 * get/put return values
 * OK	operation was successful
 * BAD	specified data was out of the accepted range
 * FAIL	specified data was in range, but doesn't exist
 * PEND	operations was initiated but not completed
 */
#define	VM_PAGER_OK	0
#define	VM_PAGER_BAD	1
#define	VM_PAGER_FAIL	2
#define	VM_PAGER_PEND	3

#define	VM_PAGER_ALLOC(h, s, p)		(*(pg)->pg_ops->pgo_alloc)(h, s, p)
#define	VM_PAGER_DEALLOC(pg)		(*(pg)->pg_ops->pgo_dealloc)(pg)
#define	VM_PAGER_GET(pg, m, s)		(*(pg)->pg_ops->pgo_getpage)(pg, m, s)
#define	VM_PAGER_PUT(pg, m, s)		(*(pg)->pg_ops->pgo_putpage)(pg, m, s)
#define	VM_PAGER_HASPAGE(pg, o)		(*(pg)->pg_ops->pgo_haspage)(pg, o)

#ifdef KERNEL
vm_pager_t	vm_pager_allocate();
void		vm_pager_deallocate();
int		vm_pager_get();
int		vm_pager_put();
boolean_t	vm_pager_has_page();

vm_offset_t	vm_pager_map_page();
void		vm_pager_unmap_page();
vm_pager_t	vm_pager_lookup();
void		vm_pager_sync();

extern struct pagerops *dfltpagerops;
#endif

#endif	/* _VM_PAGER_ */
