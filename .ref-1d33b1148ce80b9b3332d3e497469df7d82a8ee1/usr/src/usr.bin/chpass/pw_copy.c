/*-
 * Copyright (c) 1990, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pw_copy.c	8.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * This module is used to copy the master password file, replacing a single
 * record, by chpass(1) and passwd(1).
 */

#include <err.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>

#include <pw_util.h>
#include "pw_copy.h"

extern char *tempname;

void
pw_copy(ffd, tfd, pw)
	int ffd, tfd;
	struct passwd *pw;
{
	FILE *from, *to;
	int done;
	char *p, buf[8192];

	if (!(from = fdopen(ffd, "r")))
		pw_error(_PATH_MASTERPASSWD, 1, 1);
	if (!(to = fdopen(tfd, "w")))
		pw_error(tempname, 1, 1);

	for (done = 0; fgets(buf, sizeof(buf), from);) {
		if (!strchr(buf, '\n')) {
			warnx("%s: line too long", _PATH_MASTERPASSWD);
			pw_error(NULL, 0, 1);
		}
		if (done) {
			(void)fprintf(to, "%s", buf);
			if (ferror(to))
				goto err;
			continue;
		}
		if (!(p = strchr(buf, ':'))) {
			warnx("%s: corrupted entry", _PATH_MASTERPASSWD);
			pw_error(NULL, 0, 1);
		}
		*p = '\0';
		if (strcmp(buf, pw->pw_name)) {
			*p = ':';
			(void)fprintf(to, "%s", buf);
			if (ferror(to))
				goto err;
			continue;
		}
		(void)fprintf(to, "%s:%s:%d:%d:%s:%ld:%ld:%s:%s:%s\n",
		    pw->pw_name, pw->pw_passwd, pw->pw_uid, pw->pw_gid,
		    pw->pw_class, pw->pw_change, pw->pw_expire, pw->pw_gecos,
		    pw->pw_dir, pw->pw_shell);
		done = 1;
		if (ferror(to))
			goto err;
	}
	if (!done)
		(void)fprintf(to, "%s:%s:%d:%d:%s:%ld:%ld:%s:%s:%s\n",
		    pw->pw_name, pw->pw_passwd, pw->pw_uid, pw->pw_gid,
		    pw->pw_class, pw->pw_change, pw->pw_expire, pw->pw_gecos,
		    pw->pw_dir, pw->pw_shell);

	if (ferror(to))
err:		pw_error(NULL, 1, 1);
	(void)fclose(to);
}
