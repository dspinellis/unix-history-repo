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
static char sccsid[] = "@(#)v7.local.c	5.7 (Berkeley) %G%";
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

	strcpy(copy("/usr/spool/mail/", buf), user);
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
