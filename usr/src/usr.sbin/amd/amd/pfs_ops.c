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
 *	@(#)pfs_ops.c	5.4 (Berkeley) %G%
 *
 * $Id: pfs_ops.c,v 5.2.2.1 1992/02/09 15:08:56 jsp beta $
 *
 */

#include "am.h"

#ifdef HAS_PFS

/*
 * Program file system
 */

/*
 * Execute needs a mount and unmount command.
 */
static char *pfs_match(fo)
am_opts *fo;
{
	char *prog;
	if (!fo->opt_mount || !fo->opt_unmount) {
		plog(XLOG_USER, "program: no mount/unmount specified");
		return 0;
	}
	prog = strchr(fo->opt_mount, ' ');
	return strdup(prog ? prog+1 : fo->opt_mount);
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
	xivec = strsplit(info, ' ', '\'');
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
	free((voidp) info);
	free((voidp) xivec);
	/*
	 * Return error
	 */
	return error;
}

static int pfs_fmount(mf)
mntfs *mf;
{
	return pfs_exec(mf->mf_fo->opt_mount);
}

static int pfs_fumount(mf)
mntfs *mf;
{
	return pfs_exec((char *) mf->mf_private);
}

/*
 * Ops structure
 */
am_ops pfs_ops = {
	"program",
	pfs_match,
	pfs_init,
	auto_fmount,
	pfs_fmount,
	auto_fumount,
	pfs_fumount,
	efs_lookuppn,
	efs_readdir,
	0, /* pfs_readlink */
	0, /* pfs_mounted */
	0, /* pfs_umounted */
	find_afs_srvr,
	FS_BACKGROUND|FS_AMQINFO
};

#endif /* HAS_PFS */
