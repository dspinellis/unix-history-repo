/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)seteuid.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

seteuid(euid)
	int euid;
{

	return (setreuid(-1, euid));
}
