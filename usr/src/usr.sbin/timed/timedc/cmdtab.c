/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	2.6 (Berkeley) %G%";
#endif /* not lint */

#include "timedc.h"

int	clockdiff(), help(), msite(), quit(), testing(), tracing();

char	clockdiffhelp[] =	"measures clock differences between machines";
char	helphelp[] =		"gets help on commands";
char	msitehelp[] =		"finds location of master";
char	quithelp[] =		"exits timedc";
char	testinghelp[] =		"causes election timers to expire";
char	tracinghelp[] =		"turns tracing on or off";

struct cmd cmdtab[] = {
	{ "clockdiff",	clockdiffhelp,	clockdiff,	0 },
	{ "election",	testinghelp,	testing,	1 },
	{ "help",	helphelp,	help,		0 },
	{ "msite",	msitehelp,	msite,		0 },
	{ "quit",	quithelp,	quit,		0 },
	{ "trace",	tracinghelp,	tracing,	1 },
	{ "?",		helphelp,	help,		0 },
};

int	NCMDS = sizeof (cmdtab) / sizeof (cmdtab[0]);
