/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)v7.local.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * Mail -- a mail program
 *
 * Version 7
 *
 * Local routines that are installation dependent.
 */

#include "rcv.h"

/*
 * Locate the user's mailbox file (ie, the place where new, unread
 * mail is queued).  In Version 7, it is in /usr/spool/mail/name.
 */

findmail()
{

	strcpy(copy("/usr/spool/mail/", mailname), myname);
	if (isdir(mailname)) {
		stradd(mailname, '/');
		strcat(mailname, myname);
	}
}

/*
 * Get rid of the queued mail.
 */

demail()
{

	if (value("keep") != NOSTR)
		close(creat(mailname, 0666));
	else {
		if (remove(mailname) < 0)
			close(creat(mailname, 0666));
	}
}

/*
 * Discover user login name.
 */

username(uid, namebuf)
	char namebuf[];
{
	register char *np;

	if (uid == getuid() && (np = getenv("USER")) != NOSTR) {
		strncpy(namebuf, np, PATHSIZE);
		return(0);
	}
	return(getname(uid, namebuf));
}
