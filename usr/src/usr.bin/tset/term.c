/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)term.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <ttyent.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

char    tbuf[1024];      		/* Termcap entry. */

char	*askuser __P((char *));
char	*ttys __P((char *));

/*
 * Figure out what kind of terminal we're dealing with, and then read in
 * its termcap entry.
 */
char *
get_termcap_entry(userarg, tcapbufp)
	char *userarg, **tcapbufp;
{
	struct ttyent *t;
	int rval;
	char *p, *ttype, *ttypath;

	if (userarg) {
		ttype = userarg;
		goto found;
	}

	/* Try the environment. */
	if (ttype = getenv("TERM"))
		goto map;

	/* Try ttyname(3); check for dialup or other mapping. */
	if (ttypath = ttyname(STDERR_FILENO)) {
		if (p = rindex(ttypath, '/'))
			++p;
		else
			p = ttypath;
		if ((t = getttynam(p))) {
			ttype = t->ty_type;
			goto map;
		}
	}

	/* If still undefined, use "unknown". */
	ttype = "unknown";

map:	ttype = mapped(ttype);

	/*
	 * If not a path, remove TERMCAP from the environment so we get a
	 * real entry from /etc/termcap.  This prevents us from being fooled
	 * by out of date stuff in the environment.
	 */
found:	if ((p = getenv("TERMCAP")) != NULL && *p != '/')
		unsetenv("TERMCAP");

	/*
	 * ttype now contains a pointer to the type of the terminal.
	 * If the first character is '?', ask the user.
	 */
	if (ttype[0] == '?')
		if (ttype[1] != '\0')
			ttype = askuser(ttype + 1);
		else
			ttype = askuser(NULL);

	/* Find the termcap entry.  If it doesn't exist, ask the user. */
	while ((rval = tgetent(tbuf, ttype)) == 0) {
		(void)fprintf(stderr,
		    "tset: terminal type %s is unknown\n", ttype);
		ttype = askuser(NULL);
	}
	if (rval == -1)
		err("termcap: %s", strerror(errno ? errno : ENOENT));
	*tcapbufp = tbuf;
	return (ttype);
}

/* Prompt the user for a terminal type. */
char *
askuser(dflt)
	char *dflt;
{
	static char answer[256];
	char *p;

	/* We can get recalled; if so, don't continue uselessly. */
	if (feof(stdin) || ferror(stdin)) {
		(void)fprintf(stderr, "\n");
		exit(1);
	}
	for (;;) {
		if (dflt)
			(void)fprintf(stderr, "Terminal type? [%s] ", dflt);
		else
			(void)fprintf(stderr, "Terminal type? ");
		(void)fflush(stderr);

		if (fgets(answer, sizeof(answer), stdin) == NULL) {
			if (dflt == NULL) {
				(void)fprintf(stderr, "\n");
				exit(1);
			}
			return (dflt);
		}

		if (p = index(answer, '\n'))
			*p = '\0';
		if (answer[0])
			return (answer);
		if (dflt != NULL)
			return (dflt);
	}
}
