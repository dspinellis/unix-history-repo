/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1993 Jan-Simon Pendry
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)procfs_machdep.c	8.2 (Berkeley) %G%
 *
 * From:
 *	$Id: procfs_i386.c,v 3.2 1993/12/15 09:40:17 jsp Exp $
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
/*#include <machine/frame.h>*/
#include <miscfs/procfs/procfs.h>

int
procfs_read_regs(p, regs)
	struct proc *p;
	struct reg *regs;
{
	struct frame *f;

	if ((p->p_flag & P_INMEM) == 0)
		return (EIO);

	f = (struct frame *) p->p_md.md_regs;
	bcopy((void *) f->f_regs, (void *) regs->r_regs, sizeof(regs->r_regs));
	regs->r_pc = f->f_pc;
	regs->r_sr = f->f_sr;

	return (0);
}

/*
 * Update the process's current register
 * set.  Depending on the architecture this
 * may have fix-up work to do, especially
 * if the IAR or PCW are modified.
 */
int
procfs_write_regs(p, regs)
	struct proc *p;
	struct reg *regs;
{
	struct frame *f;

	if ((p->p_flag & P_INMEM) == 0)
		return (EIO);

	f = (struct frame *) p->p_md.md_regs;
	bcopy((void *) regs->r_regs, (void *) f->f_regs, sizeof(f->f_regs));
	f->f_pc = regs->r_pc;
	f->f_sr = regs->r_sr;

	return (0);
}

int
procfs_sstep(p)
	struct proc *p;
{
	int error;
	struct reg r;

	error = procfs_read_regs(p, &r);
	if (error == 0) {
		r.r_sr |= PSL_T;
		error = procfs_write_regs(p, &r);
	}

	return (error);
}

void
procfs_fix_sstep(p)
	struct proc *p;
{
}
