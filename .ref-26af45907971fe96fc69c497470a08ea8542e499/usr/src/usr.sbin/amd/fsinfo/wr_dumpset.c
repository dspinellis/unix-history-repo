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
 *	@(#)wr_dumpset.c	5.4 (Berkeley) %G%
 *
 * $Id: wr_dumpset.c,v 5.2.2.1 1992/02/09 15:09:47 jsp beta $
 *
 */

#include "../fsinfo/fsinfo.h"

static int write_dumpset_info(ef, q)
FILE *ef;
qelem *q;
{
	int errors = 0;
	disk_fs *dp;

	ITER(dp, disk_fs, q) {
		if (dp->d_dumpset) {
			fprintf(ef, "%s\t%s:%-30s\t# %s\n",
				dp->d_dumpset,
				dp->d_host->h_lochost ?
				dp->d_host->h_lochost :
				dp->d_host->h_hostname,
				dp->d_mountpt,
				dp->d_dev);
		}
	}
	return errors;
}

int write_dumpset(q)
qelem *q;
{
	int errors = 0;

	if (dumpset_pref) {
		FILE *ef = pref_open(dumpset_pref, "dumpsets", info_hdr, "exabyte dumpset");
		if (ef) {
			host *hp;
			ITER(hp, host, q) {
				if (hp->h_disk_fs) {
					errors += write_dumpset_info(ef, hp->h_disk_fs);
				}
			}
			errors += pref_close(ef);
		} else {
			errors++;
		}
	}

	return errors;
}
