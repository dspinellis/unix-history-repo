/*
 * Copyright (c) 1993 Jan-Simon Pendry
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)procfs_regs.c	8.3 (Berkeley) %G%
 *
 * From:
 *	$Id: procfs_regs.c,v 3.2 1993/12/15 09:40:17 jsp Exp $
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <machine/reg.h>
#include <miscfs/procfs/procfs.h>

int
procfs_doregs(curp, p, pfs, uio)
	struct proc *curp;
	struct proc *p;
	struct pfsnode *pfs;
	struct uio *uio;
{
	int error;
	struct reg r;
	char *kv;
	int kl;

	kl = sizeof(r);
	kv = (char *) &r;

	kv += uio->uio_offset;
	kl -= uio->uio_offset;
	if (kl > uio->uio_resid)
		kl = uio->uio_resid;

	if (kl < 0)
		error = EINVAL;
	else
		error = procfs_read_regs(p, &r);
	if (error == 0)
		error = uiomove(kv, kl, uio);
	if (error == 0 && uio->uio_rw == UIO_WRITE) {
		if (p->p_stat != SSTOP)
			error = EBUSY;
		else
			error = procfs_write_regs(p, &r);
	}

	uio->uio_offset = 0;
	return (error);
}
