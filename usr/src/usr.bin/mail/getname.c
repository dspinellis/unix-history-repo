#ifndef lint
static char sccsid[] = "@(#)getname.c	2.3 (Berkeley) %G%";
#endif
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
 * If -1 is passed as the user id, close the passwd file.
 */

getname(uid, namebuf)
	char namebuf[];
{
	struct passwd *pw;

	if (uid == -1) {
		return(0);
	}
	if ((pw = getpwuid(uid)) == NULL)
		return(-1);
	strcpy(namebuf, pw->pw_name);
	return 0;
}

/*
 * Convert the passed name to a user id and return it.  Return -1
 * on error.  Iff the name passed is -1 (yech) close the pwfile.
 */

getuserid(name)
	char name[];
{
	struct passwd *pw;

	if (name == (char *) -1) {
		return(0);
	}
	if ((pw = getpwnam(name)) == NULL)
		return 0;
	return pw->pw_uid;
}
