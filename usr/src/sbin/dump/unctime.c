/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)unctime.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <stdio.h>
#include <time.h>
#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#endif

#ifndef __P
#include <sys/cdefs.h>
#endif

/*
 * Convert a ctime(3) format string into a system format date.
 * Return the date thus calculated.
 *
 * Return -1 if the string is not in ctime format.
 */

/*
 * Offsets into the ctime string to various parts.
 */

#define	E_MONTH		4
#define	E_DAY		8
#define	E_HOUR		11
#define	E_MINUTE	14
#define	E_SECOND	17
#define	E_YEAR		20

static	int lookup __P((char *));


time_t
unctime(str)
	char *str;
{
	struct tm then;
	char dbuf[26];

	(void) strncpy(dbuf, str, sizeof(dbuf) - 1);
	dbuf[sizeof(dbuf) - 1] = '\0';
	dbuf[E_MONTH+3] = '\0';
	if ((then.tm_mon = lookup(&dbuf[E_MONTH])) < 0)
		return (-1);
	then.tm_mday = atoi(&dbuf[E_DAY]);
	then.tm_hour = atoi(&dbuf[E_HOUR]);
	then.tm_min = atoi(&dbuf[E_MINUTE]);
	then.tm_sec = atoi(&dbuf[E_SECOND]);
	then.tm_year = atoi(&dbuf[E_YEAR]) - 1900;
	then.tm_isdst = -1;
	return(mktime(&then));
}

static char months[] =
	"JanFebMarAprMayJunJulAugSepOctNovDec";

static int
lookup(str)
	char *str;
{
	register char *cp, *cp2;

	for (cp = months, cp2 = str; *cp != '\0'; cp += 3)
		if (strncmp(cp, cp2, 3) == 0)
			return((cp-months) / 3);
	return(-1);
}
