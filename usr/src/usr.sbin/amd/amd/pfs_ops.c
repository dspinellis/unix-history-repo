/*
 * $Id: pfs_ops.c,v 5.2 90/06/23 22:19:53 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)pfs_ops.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"

#ifdef HAS_PFS

/*
 * Program file system
 */

/*
 * Execute needs a mount and unmount command.
 */
static int pfs_match(fo)
am_opts *fo;
{
	char *prog;
	if (!fo->opt_mount || !fo->opt_unmount) {
		plog(XLOG_USER, "program: no mount/unmount specified");
		return FALSE;
	}
	prog = strchr(fo->opt_mount, ' ');
	if (fo->fs_mtab = strealloc(fo->fs_mtab, prog ? prog+1 : fo->opt_mount))
		return TRUE;
	return FALSE;
}

static int pfs_init(mf)
mntfs *mf;
{
	/*
	 * Save unmount command
	 */
	if (mf->mf_refc == 1) {
		mf->mf_private = (voidp) strdup(mf->mf_fo->opt_unmount);
		mf->mf_prfree = (void (*) ()) free;
	}
	return 0;
}

static int pfs_exec(info)
char *info;
{
	char **xivec;
	int error;
	/*
	 * Split copy of command info string
	 */
	info = strdup(info);
	if (info == 0)
		return ENOBUFS;
	xivec = strsplit(info, '\'');
	/*
	 * Put stdout to stderr
	 */
	(void) fclose(stdout);
	(void) dup(fileno(logfp));
	if (fileno(logfp) != fileno(stderr)) {
		(void) fclose(stderr);
		(void) dup(fileno(logfp));
	}
	/*
	 * Try the exec
	 */
#ifdef DEBUG
	Debug(D_FULL) {
		char **cp = xivec;
		plog(XLOG_DEBUG, "executing (un)mount command...");
		while (*cp) {
	  		plog(XLOG_DEBUG, "arg[%d] = '%s'", cp-xivec, *cp);
			cp++;
		}
	}
#endif /* DEBUG */
	if (xivec[0] == 0 || xivec[1] == 0) {
		errno = EINVAL;
		plog(XLOG_USER, "1st/2nd args missing to (un)mount program");
	} else {
		(void) execv(xivec[0], xivec+1);
	}
	/*
	 * Save error number
	 */
	error = errno;
	plog(XLOG_ERROR, "exec failed: %m");

	/*
	 * Free allocate memory
	 */
	free(info);
	free(xivec);
	/*
	 * Return error
	 */
	return error;
}

static int pfs_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return pfs_exec(mf->mf_fo->opt_mount);
}

static int pfs_umount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return pfs_exec((char *) mf->mf_private);
}

/*
 * Ops structure
 */
am_ops pfs_ops = {
	"program",
	pfs_match,
	pfs_init,
	pfs_mount,
	pfs_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* pfs_readlink */
	0, /* pfs_mounted */
	0, /* pfs_umounted */
	find_afs_srvr,
	FS_BACKGROUND|FS_AMQINFO
};

#endif /* HAS_PFS */
