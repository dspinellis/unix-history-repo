/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)v7.local.c	5.9 (Berkeley) %G%";
#endif /* not lint */

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
 * mail is queued).
 */
findmail(user, buf)
	char *user;
	char *buf;
{

	strcpy(copy(_PATH_MBOX, buf), user);
}

/*
 * Get rid of the queued mail.
 */
demail()
{

	if (value("keep") != NOSTR || remove(mailname) < 0)
		close(creat(mailname, 0600));
}

/*
 * Discover user login name.
 */
char*
username()
{
	char *np;

	if ((np = getenv("USER")) != NOSTR)
		return np;
	return getname(getuid());
}
