/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getname.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <pwd.h>

/*
 * Getname / getuserid for those with
 * hashed passwd data base).
 *
 */

#include "rcv.h"

/*
 * Search the passwd file for a uid.  Return name through ref parameter
 * if found, indicating success with 0 return.  Return -1 on error.
 */
char *
getname(uid)
{
	struct passwd *pw;

	if ((pw = getpwuid(uid)) == NULL)
		return NOSTR;
	return pw->pw_name;
}

/*
 * Convert the passed name to a user id and return it.  Return -1
 * on error.
 */
getuserid(name)
	char name[];
{
	struct passwd *pw;

	if ((pw = getpwnam(name)) == NULL)
		return -1;
	return pw->pw_uid;
}
