/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)log.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include "tip.h"

#ifdef ACULOG
static	FILE *flog = NULL;

/*
 * Log file maintenance routines
 */

logent(group, num, acu, message)
	char *group, *num, *acu, *message;
{
	char *user, *timestamp;
	struct passwd *pwd;
	long t;

	if (flog == NULL)
		return;
	if (flock(fileno(flog), LOCK_EX) < 0) {
		perror("tip: flock");
		return;
	}
	if ((user = getlogin()) == NOSTR)
		if ((pwd = getpwuid(getuid())) == NOPWD)
			user = "???";
		else
			user = pwd->pw_name;
	t = time(0);
	timestamp = ctime(&t);
	timestamp[24] = '\0';
	fprintf(flog, "%s (%s) <%s, %s, %s> %s\n",
		user, timestamp, group,
#ifdef PRISTINE
		"",
#else
		num,
#endif
		acu, message);
	(void) fflush(flog);
	(void) flock(fileno(flog), LOCK_UN);
}

loginit()
{
	flog = fopen(value(LOG), "a");
	if (flog == NULL)
		fprintf(stderr, "can't open log file %s.\r\n", value(LOG));
}
#endif
