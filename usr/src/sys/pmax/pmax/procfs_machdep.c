/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1994 Jan-Simon Pendry
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 * From:
 *	$Id: procfs_i386.c,v 3.2 1993/12/15 09:40:17 jsp Exp $
 *
 *	@(#)procfs_machdep.c	8.2 (Berkeley) %G%
 */

/*
 * Functions to be implemented here are:
 *
 * procfs_read_regs(proc, regs)
 *	Get the current user-visible register set from the process
 *	and copy it into the regs structure (<machine/reg.h>).
 *	The process is stopped at the time read_regs is called.
 *
 * procfs_write_regs(proc, regs)
 *	Update the current register set from the passed in regs
 *	structure.  Take care to avoid clobbering special CPU
 *	registers or privileged bits in the PSL.
 *	The process is stopped at the time write_regs is called.
 *
 * procfs_read_fpregs, procfs_write_fpregs
 *	deal with the floating point register set, otherwise as above.
 *
 * procfs_sstep(proc)
 *	Arrange for the process to trap after executing a single instruction.
 *
 * procfs_fix_sstep(proc)
 *	Cleanup process state after executing a single-step instruction.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/vnode.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <miscfs/procfs/procfs.h>

int
procfs_read_regs(p, regs)
	struct proc *p;
	struct reg *regs;
{

	if ((p->p_flag & P_INMEM) == 0)
		return (EIO);

	bcopy((void *) p->p_md.md_regs, (void *) regs->r_regs,
		sizeof(regs->r_regs));

	return (0);
}

/*
 * Update the process's current register
 * set.
 */
int
procfs_write_regs(p, regs)
	struct proc *p;
	struct reg *regs;
{
	int oldsr;

	if ((p->p_flag & P_INMEM) == 0)
		return (EIO);

	/* no user modifiable bits in the SR register */
	oldsr = p->p_md.md_regs[SR];
	bcopy((void *) regs->r_regs, (void *) p->p_md.md_regs,
		sizeof(regs->r_regs));
	p->p_md.md_regs[SR] = oldsr;

	return (0);
}

int
procfs_read_fpregs(p, fpregs)
	struct proc *p;
	struct fpreg *fpregs;
{

	return (EOPNOTSUPP);
}

int
procfs_write_fpregs(p, fpregs)
	struct proc *p;
	struct fpreg *fpregs;
{

	return (EOPNOTSUPP);
}

int
procfs_sstep(p, sstep)
	struct proc *p;
	int sstep;
{

	if (sstep && cpu_singlestep(p))
		return (EIO);

	return (0);
}

void
procfs_fix_sstep(p)
	struct proc *p;
{
}
