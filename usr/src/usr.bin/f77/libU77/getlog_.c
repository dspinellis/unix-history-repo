/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getlog_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * get login name of user
 *
 * calling sequence:
 *	character*8 getlog, name
 *	name = getlog()
 * or
 *	call getlog (name)
 * where:
 *	name will receive the login name of the user, or all blanks if
 *	this is a detached process.
 */

char *getlogin();

getlog_(name, len)
char *name; long len;
{
	char *l = getlogin();

	b_char(l?l:" ", name, len);
}
