#ifndef lint
static char sccsid[] = "@(#)cmdtab.c	4.4 (Berkeley) 7/25/83";
#endif

/*
 * lpc -- command tables
 */

#include "lpc.h"

int	abort(), clean(), enable(), disable(), help();
int	quit(), restart(), start(), status(), stop();
int	topq();

char	aborthelp[] =	"terminate a spooling daemon immediately and disable printing";
char	cleanhelp[] =	"remove cruft files from a queue";
char	enablehelp[] =	"turn a spooling queue on";
char	disablehelp[] =	"turn a spooling queue off";
char	helphelp[] =	"get help on commands";
char	quithelp[] =	"exit lpc";
char	restarthelp[] =	"restart a spooling daemon that has died";
char	starthelp[] =	"enable printing and start a spooling daemon";
char	statushelp[] =	"show status of daemon";
char	stophelp[] =	"stop a spooling daemon after current job completes and disable printing";
char	topqhelp[] =	"put job at top of printer queue";

struct cmd cmdtab[] = {
	{ "abort",	aborthelp,	abort,		1 },
	{ "clean",	cleanhelp,	clean,		1 },
	{ "enable",	enablehelp,	enable,		1 },
	{ "exit",	quithelp,	quit,		0 },
	{ "disable",	disablehelp,	disable,	1 },
	{ "help",	helphelp,	help,		0 },
	{ "quit",	quithelp,	quit,		0 },
	{ "restart",	restarthelp,	restart,	0 },
	{ "start",	starthelp,	start,		1 },
	{ "status",	statushelp,	status,		0 },
	{ "stop",	stophelp,	stop,		1 },
	{ "topq",	topqhelp,	topq,		1 },
	{ "?",		helphelp,	help,		0 },
	{ 0 },
};

int	NCMDS = sizeof (cmdtab) / sizeof (cmdtab[0]);
