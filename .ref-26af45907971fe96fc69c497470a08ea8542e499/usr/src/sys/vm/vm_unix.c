/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: vm_unix.c 1.1 89/11/07$
 *
 *	@(#)vm_unix.c	7.4 (Berkeley) %G%
 */

/*
 * Traditional sbrk/grow interface to VM
 */
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/resourcevar.h>

#include <vm/vm.h>

struct obreak_args {
	char	*nsiz;
};
/* ARGSUSED */
int
obreak(p, uap, retval)
	struct proc *p;
	struct obreak_args *uap;
	int *retval;
{
	register struct vmspace *vm = p->p_vmspace;
	vm_offset_t new, old;
	int rv;
	register int diff;

	old = (vm_offset_t)vm->vm_daddr;
	new = round_page(uap->nsiz);
	if ((int)(new - old) > p->p_rlimit[RLIMIT_DATA].rlim_cur)
		return(ENOMEM);
	old = round_page(old + ctob(vm->vm_dsize));
	diff = new - old;
	if (diff > 0) {
		rv = vm_allocate(&vm->vm_map, &old, diff, FALSE);
		if (rv != KERN_SUCCESS) {
			uprintf("sbrk: grow failed, return = %d\n", rv);
			return(ENOMEM);
		}
		vm->vm_dsize += btoc(diff);
	} else if (diff < 0) {
		diff = -diff;
		rv = vm_deallocate(&vm->vm_map, new, diff);
		if (rv != KERN_SUCCESS) {
			uprintf("sbrk: shrink failed, return = %d\n", rv);
			return(ENOMEM);
		}
		vm->vm_dsize -= btoc(diff);
	}
	return(0);
}

/*
 * Enlarge the "stack segment" to include the specified
 * stack pointer for the process.
 */
int
grow(p, sp)
	struct proc *p;
	unsigned sp;
{
	register struct vmspace *vm = p->p_vmspace;
	register int si;

	/*
	 * For user defined stacks (from sendsig).
	 */
	if (sp < (unsigned)vm->vm_maxsaddr)
		return (0);
	/*
	 * For common case of already allocated (from trap).
	 */
	if (sp >= USRSTACK - ctob(vm->vm_ssize))
		return (1);
	/*
	 * Really need to check vs limit and increment stack size if ok.
	 */
	si = clrnd(btoc(USRSTACK-sp) - vm->vm_ssize);
	if (vm->vm_ssize + si > btoc(p->p_rlimit[RLIMIT_STACK].rlim_cur))
		return (0);
	vm->vm_ssize += si;
	return (1);
}

struct ovadvise_args {
	int	anom;
};
/* ARGSUSED */
int
ovadvise(p, uap, retval)
	struct proc *p;
	struct ovadvise_args *uap;
	int *retval;
{

	return (EINVAL);
}
