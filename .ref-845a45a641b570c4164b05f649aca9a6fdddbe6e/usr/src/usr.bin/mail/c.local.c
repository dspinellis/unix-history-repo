#

/*
 * Mail -- a mail program
 *
 * EECS Cory 11/70 Version 7.0
 *
 * Local routines that are installation dependent.
 * All fiddlers please note:  if you make careful note of
 * what you change here, I will incorporate your changes and
 * you won't have to remake them each release.
 */

static char *SccsId = "@(#)c.local.c	2.1 %G%";

#include "rcv.h"
#include <pwd.h>

/*
 * Locate the user's mailbox file (ie, the place where new, unread
 * mail is queued).  At Cory, it is in /usr/spool/mail/name.
 */

findmail()
{
	register char *cp;

	cp = copy("/usr/spool/mail/", mailname);
	copy(myname, cp);
}

/*
 * Get rid of the queued mail.
 */

demail()
{
	register int p;
	
	close(creat(mailname, 0666));
	alter(mailname);
}

/*
 * Mail file lock / unlock.
 * Insignificant on Cory version 7, since /usr/spool/mail not
 * generally writable.
 */

lock(name)
	char *name;
{

	return(0);
}

unlock()
{

	return(0);
}

/*
 * Discover user login name.
 */

username(uid, namebuf)
	char namebuf[];
{
	register char *np;

	if (uid == getuid() && (np = getenv("USER")) != NOSTR) {
		strncpy(namebuf, np, 9);
		return(0);
	}
	return(getname(uid, namebuf));
}

/*
 * Discover user name from uid.  Uses the fancy hashed passwd
 * data base available only on Cory Unix.
 */

getname(uid, namebuf)
	char namebuf[];
{
	struct passwd *gp;
	struct passwd *getpwuid();

	gp = getpwuid(uid);
	if (gp == (struct passwd *) 0)
		return(-1);
	strcpy(namebuf, gp->pw_name);
	return(0);
}

/*
 * Cory hall getuserid
 */

getuserid(name)
	char name[];
{
	struct passwd *gp;
	struct passwd *getpwnam();

	if ((gp = getpwnam(name)) == (struct passwd *) 0)
		return(-1);
	return(gp->pw_uid);
}
