/*
 * Copyright (c) 1980 Regents of the University of California.
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

#ifndef lint
static char sccsid[] = "@(#)getname.c	5.8 (Berkeley) 6/1/90";
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
