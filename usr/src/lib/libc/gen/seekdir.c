/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)seekdir.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <dirent.h>

/*
 * seek to an entry in a directory.
 * Only values returned by "telldir" should be passed to seekdir.
 */
void
seekdir(dirp, loc)
	register DIR *dirp;
	long loc;
{
	register struct ddloc *lp;
	struct dirent *dp;
	extern long lseek();

	lp = dirp->dd_hash[LOCHASH(loc)];
	while (lp != NULL) {
		if (lp->loc_index == loc)
			break;
		lp = lp->loc_next;
	}
	if (lp == NULL)
		return;
	if (lp->loc_loc == dirp->dd_loc && lp->loc_seek == dirp->dd_seek)
		return;
	(void) lseek(dirp->dd_fd, lp->loc_seek, 0);
	dirp->dd_loc = 0;
	while (dirp->dd_loc < lp->loc_loc) {
		dp = readdir(dirp);
		if (dp == NULL)
			return;
	}
}
