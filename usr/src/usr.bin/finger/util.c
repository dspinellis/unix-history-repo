/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tony Nardo of the Johns Hopkins University/Applied Physics Lab.
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
static char sccsid[] = "@(#)util.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include "finger.h"
#include "pathnames.h"

find_idle_and_ttywrite(pn)
	register PERSON *pn;
{
	extern time_t now;
	extern int errno;
	struct stat sb;
	char *strerror();

	(void)sprintf(tbuf, "%s/%s", _PATH_DEV, pn->tty);
	if (stat(tbuf, &sb) < 0) {
		(void)fprintf(stderr,
		    "finger: %s: %s\n", tbuf, strerror(errno));
		exit(1);
	}
	pn->idletime = now < sb.st_atime ? 0 : now - sb.st_atime;

#define	TALKABLE	0220		/* tty is writable if 220 mode */
	pn->writable = ((sb.st_mode & TALKABLE) == TALKABLE);
}

userinfo(pn, pw)
	register PERSON *pn;
	register struct passwd *pw;
{
	register char *p, *t;
	char name[256];

	pn->realname = pn->office = pn->officephone = pn->homephone = NULL;

	pn->uid = pw->pw_uid;
	pn->name = strdup(pw->pw_name);
	pn->dir = strdup(pw->pw_dir);
	pn->shell = strdup(pw->pw_shell);

	/* why do we skip asterisks!?!? */
	(void)strcpy(p = tbuf, pw->pw_gecos);
	if (*p == '*')
		++p;

	/* ampersands get replaced by the login name */
	if (!(p = strsep(p, ",")))
		return;
	for (t = name; *t = *p; ++p)
		if (*t == '&') {
			(void)strcpy(t, pw->pw_name);
			if (islower(*t))
				*t = toupper(*t);
			while (*++t);
		}
		else
			++t;
	pn->realname = strdup(name);
	pn->office = (p = strsep((char *)NULL, ",")) ? strdup(p) : NULL;
	pn->officephone = (p = strsep((char *)NULL, ",")) ? strdup(p) : NULL;
	pn->homephone = (p = strsep((char *)NULL, ",")) ? strdup(p) : NULL;
}

match(pw, user)
	struct passwd *pw;
	char *user;
{
	register char *p, *t;
	char name[256];

	/* why do we skip asterisks!?!? */
	(void)strcpy(p = tbuf, pw->pw_gecos);
	if (*p == '*')
		++p;

	/* ampersands get replaced by the login name */
	if (!(p = strtok(p, ",")))
		return(0);
	for (t = name; *t = *p; ++p)
		if (*t == '&') {
			(void)strcpy(t, pw->pw_name);
			while (*++t);
		}
		else
			++t;
	for (t = name; p = strtok(t, "\t "); t = (char *)NULL)
		if (!strcasecmp(p, user))
			return(1);
	return(0);
}

find_when(pn)
	register PERSON *pn;
{
	static int fd;
	struct lastlog ll;
	off_t lseek();

	if (!fd && (fd = open(_PATH_LASTLOG, O_RDONLY, 0)) < 0) {
		(void)fprintf(stderr,
		    "finger: %s: open error\n", _PATH_LASTLOG);
		exit(1);
	}
	(void)lseek(fd, (long)(pn->uid * (sizeof(ll))), L_SET);
	if (read(fd, (char *)&ll, sizeof(ll)) != sizeof(ll)) {
		(void)fprintf(stderr,
		    "finger: %s: read error\n", _PATH_LASTLOG);
		exit(1);
	}
	bcopy(ll.ll_line, pn->tty, UT_LINESIZE);
	pn->tty[UT_LINESIZE] = NULL;
	bcopy(ll.ll_host, pn->host, UT_HOSTSIZE);
	pn->host[UT_HOSTSIZE] = NULL;
	pn->loginat = ll.ll_time;
}

utcopy(ut, pn)
	struct utmp *ut;
	PERSON *pn;
{
	bcopy(ut->ut_line, pn->tty, UT_LINESIZE);
	pn->tty[UT_LINESIZE] = 0;
	bcopy(ut->ut_host, pn->host, UT_HOSTSIZE);
	pn->host[UT_HOSTSIZE] = 0;
	pn->loginat = (time_t)ut->ut_time;
}
