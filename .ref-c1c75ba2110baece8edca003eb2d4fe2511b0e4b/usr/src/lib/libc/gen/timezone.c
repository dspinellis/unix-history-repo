/*
 * Copyright (c) 1987 Regents of the University of California.
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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)timezone.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <tzfile.h>

/*
 * timezone --
 *	The arguments are the number of minutes of time you are westward
 *	from Greenwich and whether DST is in effect.  It returns a string
 *	giving the name of the local timezone.  Should be replaced, in the
 *	application code, by a call to localtime.
 */

static char	czone[TZ_MAX_CHARS];		/* space for zone name */

char *
timezone(zone, dst)
	int	zone,
		dst;
{
	register char	*beg,
			*end;
	char	*getenv(), *index(), *strncpy(), *_tztab();

	if (beg = getenv("TZNAME")) {		/* set in environment */
		if (end = index(beg, ',')) {	/* "PST,PDT" */
			if (dst)
				return(++end);
			*end = '\0';
			(void)strncpy(czone,beg,sizeof(czone) - 1);
			czone[sizeof(czone) - 1] = '\0';
			*end = ',';
			return(czone);
		}
		return(beg);
	}
	return(_tztab(zone,dst));	/* default: table or created zone */
}

static struct zone {
	int	offset;
	char	*stdzone;
	char	*dlzone;
} zonetab[] = {
	-1*60,	"MET",	"MET DST",	/* Middle European */
	-2*60,	"EET",	"EET DST",	/* Eastern European */
	4*60,	"AST",	"ADT",		/* Atlantic */
	5*60,	"EST",	"EDT",		/* Eastern */
	6*60,	"CST",	"CDT",		/* Central */
	7*60,	"MST",	"MDT",		/* Mountain */
	8*60,	"PST",	"PDT",		/* Pacific */
#ifdef notdef
	/* there's no way to distinguish this from WET */
	0,	"GMT",	0,		/* Greenwich */
#endif
	0*60,	"WET",	"WET DST",	/* Western European */
	-10*60,	"EST",	"EST",		/* Aust: Eastern */
     -10*60+30,	"CST",	"CST",		/* Aust: Central */
	-8*60,	"WST",	0,		/* Aust: Western */
	-1
};

/*
 * _tztab --
 *	check static tables or create a new zone name; broken out so that
 *	we can make a guess as to what the zone is if the standard tables
 *	aren't in place in /etc.  DO NOT USE THIS ROUTINE OUTSIDE OF THE
 *	STANDARD LIBRARY.
 */
char *
_tztab(zone,dst)
	register int	zone;
	int	dst;
{
	register struct zone	*zp;
	register char	sign;

	for (zp = zonetab; zp->offset != -1;++zp)	/* static tables */
		if (zp->offset == zone) {
			if (dst && zp->dlzone)
				return(zp->dlzone);
			if (!dst && zp->stdzone)
				return(zp->stdzone);
		}

	if (zone < 0) {					/* create one */
		zone = -zone;
		sign = '+';
	}
	else
		sign = '-';
	(void)sprintf(czone,"GMT%c%d:%02d",sign,zone / 60,zone % 60);
	return(czone);
}
