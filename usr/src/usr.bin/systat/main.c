/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)main.c	5.10 (Berkeley) %G%";
#endif not lint

#include "systat.h"
#include <varargs.h>

static struct nlist nlst[] = {
#define X_FIRST		0
#define	X_HZ		0
	{ "_hz" },
#define	X_PHZ		1
	{ "_phz" },
	{ "" }
};

int	naptime = 5;

void	die(), display(), suspend();

sig_t	sigtstpdfl;
int     dellave;

static	WINDOW *wload;			/* one line window for load average */

int     verbose = 1;                    /* to report kvm read errs */

main(argc, argv)
	int argc;
	char **argv;
{

	argc--, argv++;
	while (argc > 0) {
		if (argv[0][0] == '-') {
			struct cmdtab *p;

			p = lookup(&argv[0][1]);
			if (p == (struct cmdtab *)-1) {
				fprintf(stderr, "%s: unknown request\n",
				    &argv[0][1]);
				exit(1);
			}
			curcmd = p;
		} else {
			naptime = atoi(argv[0]);
			if (naptime <= 0)
				naptime = 5;
		}
		argc--, argv++;
	}
	kvm_nlist(nlst);
	if (nlst[X_FIRST].n_type == 0) {
		fprintf(stderr, "Couldn't namelist.\n");
		exit(1);
	}
	signal(SIGINT, die);
	signal(SIGQUIT, die);
	signal(SIGTERM, die);

	/*
	 * Initialize display.  Load average appears in a one line
	 * window of its own.  Current command's display appears in
	 * an overlapping sub-window of stdscr configured by the display
	 * routines to minimize update work by curses.
	 */
	initscr();
	CMDLINE = LINES - 1;
	wnd = (*curcmd->c_open)();
	if (wnd == NULL) {
		fprintf(stderr, "Couldn't initialize display.\n");
		die();
	}
	wload = newwin(1, 0, 3, 20);
	if (wload == NULL) {
		fprintf(stderr, "Couldn't set up load average window.\n");
		die();
	}
	gethostname(hostname, sizeof (hostname));
	NREAD(X_HZ, &hz, LONG);
	NREAD(X_PHZ, &phz, LONG);
	(*curcmd->c_init)();
	curcmd->c_flags |= CF_INIT;
	labels();

	dellave = 0.0;

	signal(SIGALRM, display);
	sigtstpdfl = signal(SIGTSTP, suspend);
	display();
	noecho();
	crmode();
	keyboard();
	/*NOTREACHED*/
}

labels()
{
	if (curcmd->c_flags & CF_LOADAV) {
		mvaddstr(2, 20,
		    "/0   /1   /2   /3   /4   /5   /6   /7   /8   /9   /10");
		mvaddstr(3, 5, "Load Average");
	}
	(*curcmd->c_label)();
#ifdef notdef
	mvprintw(21, 25, "CPU usage on %s", hostname);
#endif
	refresh();
}

void
display()
{
	register int i, j;

	/* Get the load average over the last minute. */
	(void) getloadavg(avenrun, sizeof(avenrun) / sizeof(avenrun[0]));
	(*curcmd->c_fetch)();
	if (curcmd->c_flags & CF_LOADAV) {
		j = 5.0*avenrun[0] + 0.5;
		dellave -= avenrun[0];
		if (dellave >= 0.0)
			c = '<';
		else {
			c = '>';
			dellave = -dellave;
		}
		if (dellave < 0.1)
			c = '|';
		dellave = avenrun[0];
		wmove(wload, 0, 0); wclrtoeol(wload);
		for (i = (j > 50) ? 50 : j; i > 0; i--)
			waddch(wload, c);
		if (j > 50)
			wprintw(wload, " %4.1f", avenrun[0]);
	}
	(*curcmd->c_refresh)();
	if (curcmd->c_flags & CF_LOADAV)
		wrefresh(wload);
	wrefresh(wnd);
	move(CMDLINE, col);
	refresh();
	alarm(naptime);
}

load()
{

	(void) getloadavg(avenrun, sizeof(avenrun)/sizeof(avenrun[0]));
	mvprintw(CMDLINE, 0, "%4.1f %4.1f %4.1f",
	    avenrun[0], avenrun[1], avenrun[2]);
	clrtoeol();
}

void
die()
{
	move(CMDLINE, 0);
	clrtoeol();
	refresh();
	endwin();
	exit(0);
}

error(va_alist)
	va_dcl
{
	va_list ap;
	char *fmt, msg[200];

	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vsnprintf(msg, sizeof msg, fmt, ap);
	va_end(ap);
	mvaddstr(CMDLINE, 0, msg);
	clrtoeol();
	refresh();
}
