/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setgid.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Backwards compatible setgid.
 */
setgid(gid)
	int gid;
{

	return (setregid(gid, gid));
}
