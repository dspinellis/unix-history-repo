/*
 * Copyright (c) 1988 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <tzfile.h>
#include <pwd.h>
#include <stdio.h>
#include <strings.h>
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

/*
 * print --
 *	print out the file for the user to edit; strange side-effect:
 *	return if the user is allowed to modify their shell.
 */
print(fp, pw)
	FILE *fp;
	struct passwd *pw;
{
	register char *p;
	int shellval;
	char *getusershell(), *ok_shell(), *ttoa();

	shellval = 1;
	(void)fprintf(fp, "#Changing user database information for %s.\n",
	    pw->pw_name);
	if (!uid) {
		(void)fprintf(fp, "Login: %s\n", pw->pw_name);
		(void)fprintf(fp, "Password: %s\n", pw->pw_passwd);
		(void)fprintf(fp, "Uid [#]: %d\n", pw->pw_uid);
		(void)fprintf(fp, "Gid [# or name]: %d\n", pw->pw_gid);
		(void)fprintf(fp, "Change [month day year]: %s\n",
		    ttoa(pw->pw_change));
		(void)fprintf(fp, "Expire [month day year]: %s\n",
		    ttoa(pw->pw_expire));
		(void)fprintf(fp, "Class: %s\n", pw->pw_class);
		(void)fprintf(fp, "Home directory: %s\n", pw->pw_dir);
		(void)fprintf(fp, "Shell: %s\n",
		    *pw->pw_shell ? pw->pw_shell : _PATH_BSHELL);
	}
	/* only admin can change "restricted" shells */
	else if (ok_shell(pw->pw_shell))
		(void)fprintf(fp, "Shell: %s\n",
		    *pw->pw_shell ? pw->pw_shell : _PATH_BSHELL);
	else
		shellval = 0;
	p = strsep(pw->pw_gecos, ",");
	(void)fprintf(fp, "Full Name: %s\n", p ? p : "");
	p = strsep((char *)NULL, ",");
	(void)fprintf(fp, "Location: %s\n", p ? p : "");
	p = strsep((char *)NULL, ",");
	(void)fprintf(fp, "Office Phone: %s\n", p ? p : "");
	p = strsep((char *)NULL, ",");
	(void)fprintf(fp, "Home Phone: %s\n", p ? p : "");
	return(shellval);
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
