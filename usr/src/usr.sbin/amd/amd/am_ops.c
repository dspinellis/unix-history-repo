/*
 * $Id: am_ops.c,v 5.2 90/06/23 22:19:19 jsp Rel $
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
 *	@(#)am_ops.c	5.1 (Berkeley) %G%
 */

#include "am.h"

static am_ops *vops[] = {
#ifdef HAS_UFS
	&ufs_ops,
#endif /* HAS_UFS */
#ifdef HAS_NFS
	&nfs_ops,
#endif /* HAS_NFS */
#ifdef HAS_HOST
	&host_ops,
#endif /* HAS_HOST */
#ifdef HAS_SFS
	&sfs_ops,
#endif /* HAS_SFS */
#ifdef HAS_LOFS
	&lofs_ops,
#endif /* HAS_LOFS */
#ifdef HAS_PFS
	&pfs_ops,
#endif /* HAS_PFS */
	&afs_ops,	/* These three should be last ... */
	&dfs_ops,	/* ... */
	&efs_ops,	/* ... in the order afs; dfs; efs */
	0
};

#ifdef SUNOS4_COMPAT
/*
 * Crack a SunOS4-style host:fs:sub-link line
 * Construct an amd-style line and call the
 * normal amd matcher.
 */
am_ops *sunos4_match(fo, key, g_key, path, keym, map)
am_opts *fo;
char *key;
char *g_key;
char *path;
char *keym;
char *map;
{
	char *host = key;
	char *fs = strchr(host, ':');
	char *sublink = fs ? strchr(fs+1, ':') : 0;
	char keybuf[MAXPATHLEN];

	sprintf(keybuf, "type:=nfs;rhost:=%s;rfs:=%s;sublink:=%s;opts:=%s", host,
		fs ? fs+1 : "",
		sublink ? sublink+1  : "",
		g_key);
	return ops_match(fo, keybuf, "", path, keym, map);
}
#endif /* SUNOS4_COMPAT */

am_ops *ops_match(fo, key, g_key, path, keym, map)
am_opts *fo;
char *key;
char *g_key;
char *path;
char *keym;
char *map;
{
	am_ops **vp;
	am_ops *rop = 0;

	/*
	 * First crack the global opts and the local opts
	 */
	if (!eval_fs_opts(fo, key, g_key, path, keym, map)) {
		rop = &efs_ops;
	} else if (fo->opt_type == 0) {
		plog(XLOG_USER, "No fs type specified (somewhere!)");
		rop = &efs_ops;
	} else {
		/*
		 * Next find the correct filesystem type
		 */
		for (vp = vops; rop = *vp; vp++)
			if (strcmp(rop->fs_type, fo->opt_type) == 0)
				break;

		if (!rop) {
			plog(XLOG_USER, "fs type \"%s\" not recognised", fo->opt_type);
			rop = &efs_ops;
		}
	}

	/*
	 * Make sure we have a default mount option.
	 * Otherwise skip past any leading '-'.
	 */
	if (fo->opt_opts == 0)
		fo->opt_opts = "rw,defaults";
	else if (*fo->opt_opts == '-')
		fo->opt_opts++;

	/*
	 * Check the filesystem is happy
	 */
	if ((*rop->fs_match)(fo))
		return rop;

	/*
	 * Return error file system
	 */
	(void) (*efs_ops.fs_match)(fo);
	return &efs_ops;
}
