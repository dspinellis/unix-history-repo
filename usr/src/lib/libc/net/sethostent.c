/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sethostent.c	6.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/*
 * These are dummy routines to allow old programs that used /etc/hosts
 * to compile and work with the BIND name server
 */

sethostent(stayopen)
{
#ifdef lint
stayopen = stayopen;
#endif
}

endhostent()	{}

sethostfile(name)
char *name;
{
#ifdef lint
name = name;
#endif
}
