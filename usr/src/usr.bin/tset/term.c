/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)term.c	5.1 (Berkeley) %G%";
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
	char *base, *ttype, *ttypath;

	if (userarg) {
		ttype = userarg;
		goto found;
	}

	/* Try the environment. */
	if (ttype = getenv("TERM"))
		goto map;

	/* Try ttyname(3); check for dialup or other mapping. */
	if (ttypath = ttyname(STDERR_FILENO)) {
		if (base = rindex(ttypath, '/'))
			++base;
		else
			base = ttypath;
		if ((t = getttynam(base))) {
			ttype = t->ty_type;
			goto map;
		}
	}

	/* If still undefined, use "unknown". */
	ttype = "unknown";

map:	ttype = mapped(ttype);

	/*
	 * Remove TERMCAP from the environment so we get a real entry from
	 * /etc/termcap.  This prevents us from being fooled by out of date
	 * stuff in the environment.
	 */
found:	unsetenv("TERMCAP");

	/*
	 * ttype now contains a pointer to the type of the terminal.
	 * If the first character is '?', ask the user.
	 */
	if (ttype[0] == '?')
		ttype = askuser(ttype + 1);

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

	for (;;) {
		if (dflt)
			(void)fprintf(stderr, "Terminal type? [%s] ", dflt);
		else
			(void)fprintf(stderr, "Terminal type? ");
		(void)fflush(stderr);

		if (fgets(answer, sizeof(answer), stdin) == NULL)
			continue;

		if (p = index(answer, '\n'))
			*p = '\0';
		return (answer[0] ? answer : dflt);
	}
}
