/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	2.6 (Berkeley) 6/1/90";
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
