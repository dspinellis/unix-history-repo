/*
 * $Id: mtab.c,v 5.2 90/06/23 22:19:44 jsp Rel $
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
 *	@(#)mtab.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"

/*
 * Firewall /etc/mtab entries
 */
void mnt_free(mp)
struct mntent *mp;
{
	free(mp->mnt_fsname);
	free(mp->mnt_dir);
	free(mp->mnt_type);
	free(mp->mnt_opts);
	free((voidp) mp);
}

/*
 * Discard memory allocated for mount list
 */
void discard_mntlist(mp)
mntlist *mp;
{
	mntlist *mp2;

	while (mp2 = mp) {
		mp = mp->mnext;
		if (mp2->mnt)
			mnt_free(mp2->mnt);
		free(mp2);
	}
}

/*
 * Throw away a mount list
 */
void free_mntlist(mp)
mntlist *mp;
{
	discard_mntlist(mp);
	unlock_mntlist();
}

/*
 * Utility routine which determines the value of a
 * numeric option in the mount options (such as port=%d).
 * Returns 0 if the option is not specified.
 */
int hasmntval(mnt, opt)
struct mntent *mnt;
char *opt;
{
	char *str = hasmntopt(mnt, opt);
	if (str) {
		char *eq = strchr(str, '=');
		if (eq)
			return atoi(eq+1);
		else
			plog(XLOG_USER, "bad numeric option \"%s\" in \"%s\"", opt, str);
	}

	return 0;
}
