/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.14 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <tzfile.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "chpass.h"
#include "pathnames.h"

static int dmsize[] =
	{ -1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static char *months[] =
	{ "January", "February", "March", "April", "May", "June",
	  "July", "August", "September", "October", "November",
	  "December", NULL };
char *
ttoa(tval)
	time_t tval;
{
	struct tm *tp;
	static char tbuf[50];

	if (tval) {
		tp = localtime(&tval);
		(void)sprintf(tbuf, "%s %d, 19%d", months[tp->tm_mon],
		    tp->tm_mday, tp->tm_year);
	}
	else
		*tbuf = '\0';
	return(tbuf);
} 

atot(p, store)
	char *p;
	time_t *store;
{
	register char *t, **mp;
	static struct tm *lt;
	time_t tval, time();
	int day, month, year;

	if (!*p) {
		*store = 0;
		return(0);
	}
	if (!lt) {
		unsetenv("TZ");
		(void)time(&tval);
		lt = localtime(&tval);
	}
	if (!(t = strtok(p, " \t")))
		goto bad;
	for (mp = months;; ++mp) {
		if (!*mp)
			goto bad;
		if (!strncasecmp(*mp, t, 3)) {
			month = mp - months + 1;
			break;
		}
	}
	if (!(t = strtok((char *)NULL, " \t,")) || !isdigit(*t))
		goto bad;
	day = atoi(t);
	if (!(t = strtok((char *)NULL, " \t,")) || !isdigit(*t))
		goto bad;
	year = atoi(t);
	if (day < 1 || day > 31 || month < 1 || month > 12 || !year)
		goto bad;
	if (year < 100)
		year += TM_YEAR_BASE;
	if (year <= EPOCH_YEAR)
bad:		return(1);
	tval = isleap(year) && month > 2;
	for (--year; year >= EPOCH_YEAR; --year)
		tval += isleap(year) ?
		    DAYSPERLYEAR : DAYSPERNYEAR;
	while (--month)
		tval += dmsize[month];
	tval += day;
	tval = tval * HOURSPERDAY * MINSPERHOUR * SECSPERMIN;
	tval -= lt->tm_gmtoff;
	*store = tval;
	return(0);
}

char *
ok_shell(name)
	register char *name;
{
	register char *p, *sh;
	char *getusershell();

	setusershell();
	while (sh = getusershell()) {
		if (!strcmp(name, sh))
			return(name);
		/* allow just shell name, but use "real" path */
		if ((p = rindex(sh, '/')) && !strcmp(name, p + 1))
			return(sh);
	}
	return(NULL);
}
