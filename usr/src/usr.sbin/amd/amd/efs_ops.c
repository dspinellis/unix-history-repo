/*
 * $Id: efs_ops.c,v 5.2 90/06/23 22:19:23 jsp Rel $
 *
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
 *	@(#)efs_ops.c	5.1 (Berkeley) %G%
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
static int efs_match(fo)
am_opts *fo;
{
	fo->fs_mtab = strealloc(fo->fs_mtab, "(error-hook)");
	return 1;
}

/*ARGSUSED*/
static int efs_mount(mp)
am_node *mp;
{
	return ENOENT;
}

/*ARGSUSED*/
static int efs_umount(mp)
am_node *mp;
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
int efs_readdir(mp, cookie, dp, ep)
am_node *mp;
nfscookie cookie;
dirlist *dp;
entry *ep;
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
	efs_mount,
	efs_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* efs_readlink */
	0, /* efs_mounted */
	0, /* efs_umounted */
	find_afs_srvr,
	FS_DISCARD
};

#endif /* HAS_EFS */
