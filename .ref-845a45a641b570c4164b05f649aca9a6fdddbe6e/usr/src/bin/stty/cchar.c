/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cchar.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "stty.h"
#include "extern.h"

/*
 * Special control characters.
 *
 * Cchars1 are the standard names, cchars2 are the old aliases.
 * The first are displayed, but both are recognized on the
 * command line.
 */
struct cchar cchars1[] = {
	{ "discard",	VDISCARD, 	CDISCARD },
	{ "dsusp", 	VDSUSP,		CDSUSP },
	{ "eof",	VEOF,		CEOF },
	{ "eol",	VEOL,		CEOL },
	{ "eol2",	VEOL2,		CEOL },
	{ "erase",	VERASE,		CERASE },
	{ "intr",	VINTR,		CINTR },
	{ "kill",	VKILL,		CKILL },
	{ "lnext",	VLNEXT,		CLNEXT },
	{ "quit",	VQUIT,		CQUIT },
	{ "reprint",	VREPRINT, 	CREPRINT },
	{ "start",	VSTART,		CSTART },
	{ "status",	VSTATUS, 	CSTATUS },
	{ "stop",	VSTOP,		CSTOP },
	{ "susp",	VSUSP,		CSUSP },
	{ "werase",	VWERASE,	CWERASE },
	{ NULL },
};

struct cchar cchars2[] = {
	{ "brk",	VEOL,		CEOL },
	{ "flush",	VDISCARD, 	CDISCARD },
	{ "rprnt",	VREPRINT, 	CREPRINT },
	{ NULL },
};

int
csearch(argvp, ip)
	char ***argvp;
	struct info *ip;
{
	register struct cchar *cp;
	struct cchar tmp;
	char *arg, *name;
	static int c_cchar __P((const void *, const void *));
		
	name = **argvp;

	tmp.name = name;
	if (!(cp = (struct cchar *)bsearch(&tmp, cchars1,
	    sizeof(cchars1)/sizeof(struct cchar) - 1, sizeof(struct cchar),
	    c_cchar)) && !(cp = (struct cchar *)bsearch(&tmp, cchars1,
	    sizeof(cchars1)/sizeof(struct cchar) - 1, sizeof(struct cchar),
	    c_cchar)))
		return (0);

	arg = *++*argvp;
	if (!arg) {
		warnx("option requires an argument -- %s", name);
		usage();
	}

#define CHK(s)  (*arg == s[0] && !strcmp(arg, s))
	if (CHK("undef") || CHK("<undef>"))
		ip->t.c_cc[cp->sub] = _POSIX_VDISABLE;
	else if (arg[0] == '^')
		ip->t.c_cc[cp->sub] = (arg[1] == '?') ? 0177 :
		    (arg[1] == '-') ? _POSIX_VDISABLE : arg[1] & 037;
	else
		ip->t.c_cc[cp->sub] = arg[0];
	ip->set = 1;
	return (1);
}

static int
c_cchar(a, b)
        const void *a, *b;
{
        return (strcmp(((struct cchar *)a)->name, ((struct cchar *)b)->name));
}
