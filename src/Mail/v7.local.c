/* Copyright (c) 1979 Regents of the University of California */
#

/*
 * Mail -- a mail program
 *
 * Version 7
 *
 * Local routines that are installation dependent.
 * All fiddlers please note:  if you make careful note of
 * what you change here, I will incorporate your changes and
 * you won't have to remake them each release.
 */

#include "rcv.h"

/*
 * Locate the user's mailbox file (ie, the place where new, unread
 * mail is queued).  In Version 7, it is in /usr/spool/mail/name.
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
	unlink(mailname);
}

/*
 * A stub for the mailfile locking stuff.
 */

lock(name)
	char name[];
{

	return(0);
}

unlock()
{

	return(0);
}
