#ifndef lint
static char sccsid[] = "@(#)sdmail.c	5.2 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <pwd.h>

/*******
 *	sdmail(file, uid)
 *
 *	sdmail  -  this routine will determine the owner
 *	of the file (file), create a message string and
 *	call "mailst" to send the cleanup message.
 *	This is only implemented for local system
 *	mail at this time.
 */

sdmail(file, uid)
char *file;
register int uid;
{
	static struct passwd *pwd = NULL;
	struct passwd *getpwuid();
	char mstr[40];

	sprintf(mstr, "uuclean deleted file %s\n", file);
	if (pwd != NULL && pwd->pw_uid == uid) {
		mailst(pwd->pw_name, mstr);
		return(0);
	}

	if ((pwd = getpwuid(uid)) != NULL) {
		mailst(pwd->pw_name, mstr);
	}
	return(0);
}


/***
 *	mailst(user, str)
 *	char *user, *str;
 *
 *	mailst  -  this routine will fork and execute
 *	a mail command sending string (str) to user (user).
 */

mailst(user, str)
char *user, *str;
{
	register FILE *fp;
	char cmd[100];

	sprintf(cmd, "mail %s", user);
	if ((fp = rpopen(cmd, "w")) == NULL)
		return;
/* \n added to mail message.  uw-beave!jim (Jim Rees) */
	fprintf(fp, "%s\n", str);
	pclose(fp);
	return;
}
