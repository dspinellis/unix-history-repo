/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)getpwinfo.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "uucp.h"
#include <pwd.h>

/*LINTLIBRARY*/

/*
 *	get passwd file info for uid
 *
 *	return codes:  SUCCESS  |  FAIL
 *
 */

guinfo(uid, name, path)
int uid;
register char *path, *name;
{
	register struct passwd *pwd;
	char *getlogin(), *getenv(), *l;

	if ((l = getlogin()) == NULL) {
		l = getenv("USER");
	}
	if (l != NULL) {
		pwd = getpwnam(l);
		if (pwd != NULL && pwd->pw_uid == uid)
			goto setup;
	}
	if ((pwd = getpwuid(uid)) == NULL) {
		/* can not find uid in passwd file */
		*name = '\0';
		*path = '\0';
		return FAIL;
	}

    setup:
	strcpy(path, pwd->pw_dir);
	strcpy(name, pwd->pw_name);
	return SUCCESS;
}


/*
 *	get passwd file info for name
 *
 *	return codes:  SUCCESS  |  FAIL
 */

gninfo(name, uid, path)
char *path, *name;
int *uid;
{
	register struct passwd *pwd;
	struct passwd *getpwnam();

	if ((pwd = getpwnam(name)) == NULL) {
		/* can not find name in passwd file */
		*path = '\0';
		return FAIL;
	}

	strcpy(path, pwd->pw_dir);
	*uid = pwd->pw_uid;
	return SUCCESS;
}
