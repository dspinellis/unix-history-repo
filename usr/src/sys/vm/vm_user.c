/* 
 * Copyright (c) 1985, Avadis Tevanian, Jr., Michael Wayne Young
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
 *	@(#)vm_user.c	7.2 (Berkeley) %G%
 */

/*
 *	User-exported virtual memory functions.
 */

#include "param.h"
#include "systm.h"
#include "proc.h"

#include "vm.h"
#include "vm_page.h"

simple_lock_data_t	vm_alloc_lock;	/* XXX */

#ifdef MACHVMCOMPAT
/*
 * BSD style syscall interfaces to MACH calls
 * All return MACH return values.
 */
/* ARGSUSED */
svm_allocate(p, uap, retval)
	struct proc *p;
	struct args {
		vm_map_t map;
		vm_offset_t *addr;
		vm_size_t size;
		boolean_t anywhere;
	} *uap;
	int *retval;
{
	vm_offset_t addr;
	int rv;

	uap->map = p->p_map;		/* XXX */

	if (copyin((caddr_t)uap->addr, (caddr_t)&addr, sizeof (addr)))
		rv = KERN_INVALID_ARGUMENT;
	else
		rv = vm_allocate(uap->map, &addr, uap->size, uap->anywhere);
	if (rv == KERN_SUCCESS) {
		if (copyout((caddr_t)&addr, (caddr_t)uap->addr, sizeof(addr)))
			rv = KERN_INVALID_ARGUMENT;
	}
	return((int)rv);
}

/* ARGSUSED */
svm_deallocate(p, uap, retval)
	struct proc *p;
	struct args {
		vm_map_t map;
		vm_offset_t addr;
		vm_size_t size;
	} *uap;
	int *retval;
{
	int rv;

	uap->map = p->p_map;		/* XXX */
	rv = vm_deallocate(uap->map, uap->addr, uap->size);
	return((int)rv);
}

/* ARGSUSED */
svm_inherit(p, uap, retval)
	struct proc *p;
	struct args {
		vm_map_t map;
		vm_offset_t addr;
		vm_size_t size;
		vm_inherit_t inherit;
	} *uap;
	int *retval;
{
	int rv;

	uap->map = p->p_map;		/* XXX */
	rv = vm_inherit(uap->map, uap->addr, uap->size, uap->inherit);
	return((int)rv);
}

/* ARGSUSED */
svm_protect(p, uap, retval)
	struct proc *p;
	struct args {
		vm_map_t map;
		vm_offset_t addr;
		vm_size_t size;
		boolean_t setmax;
		vm_prot_t prot;
	} *uap;
	int *retval;
{
	int rv;

	uap->map = p->p_map;		/* XXX */
	rv = vm_protect(uap->map, uap->addr, uap->size, uap->setmax, uap->prot);
	return((int)rv);
}
#endif

/*
 *	vm_allocate allocates "zero fill" memory in the specfied
 *	map.
 */
vm_allocate(map, addr, size, anywhere)
	register vm_map_t	map;
	register vm_offset_t	*addr;
	register vm_size_t	size;
	boolean_t		anywhere;
{
	int	result;

	if (map == NULL)
		return(KERN_INVALID_ARGUMENT);
	if (size == 0) {
		*addr = 0;
		return(KERN_SUCCESS);
	}

	if (anywhere)
		*addr = vm_map_min(map);
	else
		*addr = trunc_page(*addr);
	size = round_page(size);

	result = vm_map_find(map, NULL, (vm_offset_t) 0, addr,
			size, anywhere);

	return(result);
}

/*
 *	vm_deallocate deallocates the specified range of addresses in the
 *	specified address map.
 */
vm_deallocate(map, start, size)
	register vm_map_t	map;
	vm_offset_t		start;
	vm_size_t		size;
{
	if (map == NULL)
		return(KERN_INVALID_ARGUMENT);

	if (size == (vm_offset_t) 0)
		return(KERN_SUCCESS);

	return(vm_map_remove(map, trunc_page(start), round_page(start+size)));
}

/*
 *	vm_inherit sets the inheritence of the specified range in the
 *	specified map.
 */
vm_inherit(map, start, size, new_inheritance)
	register vm_map_t	map;
	vm_offset_t		start;
	vm_size_t		size;
	vm_inherit_t		new_inheritance;
{
	if (map == NULL)
		return(KERN_INVALID_ARGUMENT);

	return(vm_map_inherit(map, trunc_page(start), round_page(start+size), new_inheritance));
}

/*
 *	vm_protect sets the protection of the specified range in the
 *	specified map.
 */

vm_protect(map, start, size, set_maximum, new_protection)
	register vm_map_t	map;
	vm_offset_t		start;
	vm_size_t		size;
	boolean_t		set_maximum;
	vm_prot_t		new_protection;
{
	if (map == NULL)
		return(KERN_INVALID_ARGUMENT);

	return(vm_map_protect(map, trunc_page(start), round_page(start+size), new_protection, set_maximum));
}
