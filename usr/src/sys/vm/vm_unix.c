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
 *	@(#)vm_unix.c	7.1 (Berkeley) %G%
 */

/*
 * Traditional sbrk/grow interface to VM
 */
#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"

#include "../vm/vm_param.h"
#include "machine/vmparam.h"

/* ARGSUSED */
obreak(p, uap, retval)
	struct proc *p;
	struct args {
		char	*nsiz;
	} *uap;
	int *retval;
{
	vm_offset_t new, old;
	int rv;
	register int diff;

	old = (vm_offset_t)u.u_daddr;
	new = round_page(uap->nsiz);
	if ((int)(new - old) > u.u_rlimit[RLIMIT_DATA].rlim_cur)
		return(ENOMEM);
	old = round_page(old + ctob(u.u_dsize));
	diff = new - old;
	if (diff > 0) {
		rv = vm_allocate(p->p_map, &old, diff, FALSE);
		if (rv != KERN_SUCCESS) {
			uprintf("sbrk: grow failed, return = %d\n", rv);
			return(ENOMEM);
		}
		u.u_dsize += btoc(diff);
	} else if (diff < 0) {
		diff = -diff;
		rv = vm_deallocate(p->p_map, new, diff);
		if (rv != KERN_SUCCESS) {
			uprintf("sbrk: shrink failed, return = %d\n", rv);
			return(ENOMEM);
		}
		u.u_dsize -= btoc(diff);
	}
	return(0);
}

/*
 * grow the stack to include the SP
 * true return if successful.
 */
grow(sp)
	unsigned sp;
{
	register int si;

	/*
	 * For user defined stacks (from sendsig).
	 */
	if (sp < (unsigned)u.u_maxsaddr)
		return (0);
	/*
	 * For common case of already allocated (from trap).
	 */
	if (sp >= USRSTACK-ctob(u.u_ssize))
		return (1);
	/*
	 * Really need to check vs limit and increment stack size if ok.
	 */
	si = clrnd(btoc(USRSTACK-sp) - u.u_ssize);
	if (u.u_ssize+si > btoc(u.u_rlimit[RLIMIT_STACK].rlim_cur))
		return (0);
	u.u_ssize += si;
	return (1);
}

/* ARGSUSED */
ovadvise(p, uap, retval)
	struct proc *p;
	struct args {
		int	anom;
	} *uap;
	int *retval;
{

}
