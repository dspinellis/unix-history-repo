/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)sys_process.c	7.22 (Berkeley) 5/11/91
 */

#define IPCREG
#include "param.h"
#include "proc.h"
#include "vnode.h"
#include "seg.h"
#include "buf.h"
#include "ptrace.h"

#include "machine/reg.h"
#include "machine/psl.h"
#include "vm/vm.h"
#include "vm/vm_page.h"

#include "user.h"

/*
 * Process debugging system call.
 */
ptrace(curp, uap, retval)
	struct proc *curp;
	register struct args {
		int	req;
		int	pid;
		int	*addr;
		int	data;
	} *uap;
	int *retval;
{

	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}

procxmt(p)
	register struct proc *p;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * Enable process profiling system call.
 */
/* ARGSUSED */
profil(p, uap, retval)
	struct proc *p;
	register struct args {
		short	*bufbase;
		unsigned bufsize;
		unsigned pcoffset;
		unsigned pcscale;
	} *uap;
	int *retval;
{

	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}
