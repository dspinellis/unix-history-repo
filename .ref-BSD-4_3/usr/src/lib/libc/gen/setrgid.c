/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setrgid.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

setrgid(rgid)
	int rgid;
{

	return (setregid(rgid, -1));
}
