/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cchar.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stddef.h>
#include "stty.h"

/*
 * Special control characters.
 *
 * Cchars1 are the standard names, cchars2 are the old aliases.
 * The first are displayed, but both are recognized on the
 * command line.
 */
struct cchar cchars1[] = {
	"discard",	VDISCARD, 	CDISCARD,
	"dsusp", 	VDSUSP,		CDSUSP,
	"eof",		VEOF,		CEOF,
	"eol",		VEOL,		CEOL,
	"eol2",		VEOL2,		CEOL,
	"erase",	VERASE,		CERASE,
	"intr",		VINTR,		CINTR,
	"kill",		VKILL,		CKILL,
	"lnext",	VLNEXT,		CLNEXT,
	"quit",		VQUIT,		CQUIT,
	"reprint",	VREPRINT, 	CREPRINT,
	"start",	VSTART,		CSTART,
	"status",	VSTATUS, 	CSTATUS,
	"stop",		VSTOP,		CSTOP,
	"susp",		VSUSP,		CSUSP,
	"werase",	VWERASE,	CWERASE,
	NULL
};

struct cchar cchars2[] = {
	"brk",		VEOL,		CEOL,
	"flush",	VDISCARD, 	CDISCARD,
	"rprnt",	VREPRINT, 	CREPRINT,
	"xoff",		VSTOP,		CSTOP,
	"xon",		VSTART,		CSTART,
	NULL
};
