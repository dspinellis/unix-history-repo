/*
 * $Id: sfs_ops.c,v 5.2 90/06/23 22:19:59 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
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
 *	@(#)sfs_ops.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"

#ifdef HAS_SFS

/*
 * Symbol-link file system
 */

/*
 * SFS needs a link.
 */
static int sfs_match(fo)
am_opts *fo;
{
	if (!fo->opt_fs) {
		plog(XLOG_USER, "link: no fs specified");
		return 0;
	}

	/*
	 * Bug report (14/12/89) from Jay Plett <jay@princeton.edu>
	 * If an automount point has the same name as an existing
	 * link type mount Amd hits a race condition and either hangs
	 * or causes a symlink loop.
	 *
	 * If fs begins with a '/' change the opt_fs & opt_sublink
	 * fields so that the fs option doesn't end up pointing at
	 * an existing symlink.
	 *
	 * If sublink is nil then set sublink to fs
	 * else set sublink to fs / sublink
	 *
	 * Finally set fs to ".".
	 */
	if (*fo->opt_fs == '/') {
		char *fullpath;
		char *link = fo->opt_sublink;
		if (link) {
			if (*link == '/')
				fullpath = strdup(link);
			else
				fullpath = str3cat((char *)0, fo->opt_fs, "/", link);
		} else {
			fullpath = strdup(fo->opt_fs);
		}

		if (fo->opt_sublink)
			free(fo->opt_sublink);
		fo->opt_sublink = fullpath;
		free(fo->opt_fs);
		fo->opt_fs = strdup(".");
	}

	fo->fs_mtab = strealloc(fo->fs_mtab, fo->opt_fs);

	return 1;
}

/*ARGUSED*/
static int sfs_mount(mp)
am_node *mp;
{
	/*
	 * Wow - this is hard to implement!
	 */

	return 0;
}

/*ARGUSED*/
static int sfs_umount(mp)
am_node *mp;
{
	return 0;
}

/*
 * Ops structure
 */
am_ops sfs_ops = {
	"link",
	sfs_match,
	0, /* sfs_init */
	sfs_mount,
	sfs_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* sfs_readlink */
	0, /* sfs_mounted */
	0, /* sfs_umounted */
	find_afs_srvr,
#ifdef FLUSH_KERNEL_NAME_CACHE
	FS_UBACKGROUND
#else /* FLUSH_KERNEL_NAME_CACHE */
	0
#endif /* FLUSH_KERNEL_NAME_CACHE */
};

#endif /* HAS_SFS */
