/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setegid.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

setegid(egid)
	int egid;
{

	return (setregid(-1, egid));
}
