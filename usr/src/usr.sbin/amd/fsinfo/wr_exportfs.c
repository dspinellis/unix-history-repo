/*
 * $Id: wr_exportfs.c,v 5.2.1.2 90/12/21 16:46:51 jsp Alpha $
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
 *	@(#)wr_exportfs.c	5.1 (Berkeley) %G%
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
