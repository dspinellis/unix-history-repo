/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)v7.local.c	5.6 (Berkeley) %G%";
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
char *
findmail(user)
	char *user;
{
	static char buf[PATHSIZE];

	strcpy(copy("/usr/spool/mail/", buf), user);
	return buf;
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
