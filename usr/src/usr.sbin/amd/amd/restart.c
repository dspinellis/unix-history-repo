/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 * $Id: restart.c,v 5.2.2.1 1992/02/09 15:08:59 jsp beta $
 *
 */

#ifndef lint
static char sccsid[] = "@(#)restart.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include "am.h"

/*
 * Handle an amd restart.
 *
 * Scan through the mount list finding all "interesting" mount points.
 * Next hack up partial data structures and add the mounted file
 * system to the list of known filesystems.  This will leave a
 * dangling reference to that filesystems, so when the filesystem is
 * finally inherited, an extra "free" must be done on it.
 *
 * This module relies on internal details of other components.  If
 * you change something else make *sure* restart() still works.
 */
void restart()
{
	/*
	 * Read the existing mount table
	 */
	mntlist *ml, *mlp;

	/*
	 * For each entry, find nfs, ufs or auto mounts
	 * and create a partial am_node to represent it.
	 */
	for (mlp = ml = read_mtab("restart"); mlp; mlp = mlp->mnext) {
		struct mntent *me = mlp->mnt;
		am_ops *fs_ops = 0;
		if (STREQ(me->mnt_type, MTAB_TYPE_UFS)) {
			/*
			 * UFS entry
			 */
			fs_ops = &ufs_ops;
		} else if (STREQ(me->mnt_type, MTAB_TYPE_NFS)) {
			/*
			 * NFS entry, or possibly an Amd entry...
			 */
			int au_pid;
			char *colon = strchr(me->mnt_fsname, ':');
			if (colon && sscanf(colon, ":(pid%d)", &au_pid) == 1) {
				plog(XLOG_WARNING, "%s is an existing automount point", me->mnt_dir);
				fs_ops = &sfs_ops;
			} else {
				fs_ops = &nfs_ops;
			}
#ifdef MTAB_TYPE_MFS
		} else if (STREQ(me->mnt_type, MTAB_TYPE_MFS)) {
			/*
			 * MFS entry.  Fake with a symlink.
			 */
			fs_ops = &sfs_ops;
#endif /* MTAB_TYPE_MFS */
		} else {
			/*
			 * Catch everything else with symlinks to
			 * avoid recursive mounts.  This is debatable...
			 */
			fs_ops = &sfs_ops;
		}

		/*
		 * If we found something to do
		 */
		if (fs_ops) {
			mntfs *mf;
			am_opts mo;
			char *cp;
			cp = strchr(me->mnt_fsname, ':');
			/*
			 * Partially fake up an opts structure
			 */
			mo.opt_rhost = 0;
			mo.opt_rfs = 0;
			if (cp) {
				*cp = '\0';
				mo.opt_rhost = strdup(me->mnt_fsname);
				mo.opt_rfs = strdup(cp+1);
				*cp = ':';
			} else if (fs_ops->ffserver == find_nfs_srvr) {
				/* 
				 * Prototype 4.4 BSD used to end up here -
				 * might as well keep the workaround for now
				 */
				plog(XLOG_WARNING, "NFS server entry assumed to be %s:/", me->mnt_fsname);
				mo.opt_rhost = strdup(me->mnt_fsname);
				mo.opt_rfs = strdup("/");
				me->mnt_fsname = str3cat(me->mnt_fsname, mo.opt_rhost, ":", "/");
			}
			mo.opt_fs = me->mnt_dir;
			mo.opt_opts = me->mnt_opts;

			/*
			 * Make a new mounted filesystem
			 */
			mf = find_mntfs(fs_ops, &mo, me->mnt_dir,
				me->mnt_fsname, "", me->mnt_opts, "");
			if (mf->mf_refc == 1) {
				mf->mf_flags |= MFF_RESTART|MFF_MOUNTED;
				mf->mf_error = 0;	/* Already mounted correctly */
				mf->mf_fo = 0;
				/*
				 * If the restarted type is a link then
				 * don't time out.
				 */
				if (fs_ops == &sfs_ops || fs_ops == &ufs_ops)
					mf->mf_flags |= MFF_RSTKEEP;
				if (fs_ops->fs_init) {
					/*
					 * Don't care whether this worked since
					 * it is checked again when the fs is
					 * inherited.
					 */
					(void) (*fs_ops->fs_init)(mf);
				}

				plog(XLOG_INFO, "%s restarted fstype %s on %s",
					me->mnt_fsname, fs_ops->fs_type, me->mnt_dir);
			} else {
				/* Something strange happened - two mounts at the same place! */
				free_mntfs(mf);
			}
			/*
			 * Clean up mo
			 */
			if (mo.opt_rhost)
				free(mo.opt_rhost);
			if (mo.opt_rfs)
				free(mo.opt_rfs);
		}
	}

	/*
	 * Free the mount list
	 */
	free_mntlist(ml);
}
