/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getmntinfo.c	6.3 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/mount.h>

/*
 * Return information about mounted filesystems.
 */
int
getmntinfo(mntbufp, flags)
	struct statfs **mntbufp;
	int flags;
{
	static struct statfs *mntbuf;
	static int mntsize, bufsize;

	if (mntsize <= 0 && (mntsize = getfsstat(0, 0, MNT_NOWAIT)) < 0)
		return (0);
	if (bufsize > 0 && (mntsize = getfsstat(mntbuf, bufsize, flags)) < 0)
		return (0);
	while (bufsize <= mntsize * sizeof(struct statfs)) {
		if (mntbuf)
			free(mntbuf);
		bufsize = (mntsize + 1) * sizeof(struct statfs);
		if ((mntbuf = (struct statfs *)malloc(bufsize)) == 0)
			return (0);
		if ((mntsize = getfsstat(mntbuf, bufsize, flags)) < 0)
			return (0);
	}
	*mntbufp = mntbuf;
	return (mntsize);
}
