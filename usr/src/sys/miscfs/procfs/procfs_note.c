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
 *	@(#)procfs_note.c	8.2 (Berkeley) %G%
 *
 * From:
 *	$Id: procfs_note.c,v 3.2 1993/12/15 09:40:17 jsp Exp $
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/signal.h>
#include <miscfs/procfs/procfs.h>

int
procfs_donote(curp, p, pfs, uio)
	struct proc *curp;
	struct proc *p;
	struct pfsnode *pfs;
	struct uio *uio;
{
	int xlen;
	int error;
	char note[PROCFS_NOTELEN+1];

	if (uio->uio_rw != UIO_WRITE)
		return (EINVAL);

	xlen = PROCFS_NOTELEN;
	error = vfs_getuserstr(uio, note, &xlen);
	if (error)
		return (error);

	/* send to process's notify function */
	return (EOPNOTSUPP);
}
