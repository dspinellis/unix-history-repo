/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)v7.local.c	5.11 (Berkeley) 6/24/90";
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
	char *user, *buf;
{
	(void)sprintf(buf, "%s/%s", _PATH_MAILDIR, user);
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
