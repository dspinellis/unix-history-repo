/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)efs_ops.c	5.4 (Berkeley) %G%
 *
 * $Id: efs_ops.c,v 5.2.2.1 1992/02/09 15:08:21 jsp beta $
 *
 */

#include "am.h"

#ifdef HAS_EFS

/*
 * Error file system.
 * This is used as a last resort catchall if
 * nothing else worked.  EFS just returns lots
 * of error codes, except for unmount which
 * always works of course.
 */

/*
 * EFS file system always matches
 */
static char *efs_match(fo)
am_opts *fo;
{
	return strdup("(error-hook)");
}

/*ARGSUSED*/
static int efs_fmount(mf)
mntfs *mf;
{
	return ENOENT;
}

/*ARGSUSED*/
static int efs_fumount(mf)
mntfs *mf;
{
	/*
	 * Always succeed
	 */

	return 0;
}

/*
 * EFS interface to RPC lookup() routine.
 * Should never get here in the automounter.
 * If we do then just give an error.
 */
/*ARGSUSED*/
am_node *efs_lookuppn(mp, fname, error_return, op)
am_node *mp;
char *fname;
int *error_return;
int op;
{
	*error_return = ESTALE;
	return 0;
}

/*
 * EFS interface to RPC readdir() routine.
 * Should never get here in the automounter.
 * If we do then just give an error.
 */
/*ARGSUSED*/
int efs_readdir(mp, cookie, dp, ep, count)
am_node *mp;
nfscookie cookie;
dirlist *dp;
entry *ep;
int count;
{
	return ESTALE;
}

/*
 * Ops structure
 */
am_ops efs_ops = {
	"error",
	efs_match,
	0, /* efs_init */
	auto_fmount,
	efs_fmount,
	auto_fumount,
	efs_fumount,
	efs_lookuppn,
	efs_readdir,
	0, /* efs_readlink */
	0, /* efs_mounted */
	0, /* efs_umounted */
	find_afs_srvr,
	FS_DISCARD
};

#endif /* HAS_EFS */
