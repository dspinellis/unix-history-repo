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
 *	@(#)wr_exportfs.c	5.4 (Berkeley) %G%
 *
 * $Id: wr_exportfs.c,v 5.2.2.1 1992/02/09 15:09:48 jsp beta $
 *
 */

#include "../fsinfo/fsinfo.h"

static int write_export_info(ef, q, errors)
FILE *ef;
qelem *q;
int errors;
{
	mount *mp;

	ITER(mp, mount, q) {
		if (mp->m_mask & (1<<DM_EXPORTFS))
			fprintf(ef, "%s\t%s\n", mp->m_volname, mp->m_exportfs);
		if (mp->m_mount)
			errors += write_export_info(ef, mp->m_mount, 0);
	}

	return errors;
}

static int write_dkexports(ef, q)
FILE *ef;
qelem *q;
{
	int errors = 0;
	disk_fs *dp;

	ITER(dp, disk_fs, q) {
		if (dp->d_mount)
			errors += write_export_info(ef, dp->d_mount, 0);
	}
	return errors;
}

int write_exportfs(q)
qelem *q;
{
	int errors = 0;

	if (exportfs_pref) {
		host *hp;
		show_area_being_processed("write exportfs", "");
		ITER(hp, host, q) {
			if (hp->h_disk_fs) {
				FILE *ef = pref_open(exportfs_pref, hp->h_hostname, gen_hdr, hp->h_hostname);
				if (ef) {
					show_new(hp->h_hostname);
					errors += write_dkexports(ef, hp->h_disk_fs);
					errors += pref_close(ef);
				} else {
					errors++;
				}
			}
		}
	}

	return errors;
}
