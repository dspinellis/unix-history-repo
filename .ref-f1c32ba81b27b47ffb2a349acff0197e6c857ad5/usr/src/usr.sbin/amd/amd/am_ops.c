/*
 * $Id: am_ops.c,v 5.2.1.3 91/03/03 20:37:39 jsp Alpha $
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
 *	@(#)am_ops.c	5.2 (Berkeley) %G%
 */

#include "am.h"

static am_ops *vops[] = {
#ifdef HAS_UFS
	&ufs_ops,
#endif
#ifdef HAS_NFS
	&nfs_ops,
#endif
#ifdef HAS_NFSX
	&nfsx_ops,
#endif
#ifdef HAS_HOST
	&host_ops,
#endif
#ifdef HAS_SFS
	&sfs_ops,
#endif
#ifdef HAS_LOFS
	&lofs_ops,
#endif
#ifdef HAS_PFS
	&pfs_ops,
#endif
#ifdef HAS_UNION_FS
	&union_ops,
#endif
	&afs_ops,	/* These four should be last ... */
	&dfs_ops,	/* ... */
	&toplvl_ops,	/* ... */
	&efs_ops,	/* ... in the order afs; dfs; toplvl; efs */
	0
};

void ops_showfstypes P((FILE *fp));
void ops_showfstypes(fp)
FILE *fp;
{
	struct am_ops **ap;
	char *sep = "";

	for (ap = vops; *ap; ap++) {
		fprintf(fp, "%s%s", sep, (*ap)->fs_type);
		sep = ", ";
	}
}

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
		plog(XLOG_USER, "No fs type specified (key = \"%s\", map = \"%s\")", keym, map);
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
	if (fo->fs_mtab)
		free((voidp) fo->fs_mtab);

	if (fo->fs_mtab = (*rop->fs_match)(fo))
		return rop;

	/*
	 * Return error file system
	 */
	fo->fs_mtab = (*efs_ops.fs_match)(fo);
	return &efs_ops;
}
