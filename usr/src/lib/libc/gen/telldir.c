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
static char sccsid[] = "@(#)telldir.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <dirent.h>

/*
 * return a pointer into a directory
 */
long
telldir(dirp)
	DIR *dirp;
{
	register int index;
	register struct ddloc *lp;

	if ((lp = (struct ddloc *)malloc(sizeof(struct ddloc))) == NULL)
		return (-1);
	index = dirp->dd_loccnt++;
	lp->loc_index = index;
	lp->loc_seek = dirp->dd_seek;
	lp->loc_loc = dirp->dd_loc;
	lp->loc_next = dirp->dd_hash[LOCHASH(index)];
	dirp->dd_hash[LOCHASH(index)] = lp;
	return (index);
}
