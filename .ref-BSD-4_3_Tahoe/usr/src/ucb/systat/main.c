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
static char sccsid[] = "@(#)main.c	5.3 (Berkeley) 3/26/87";
#endif not lint

#include "systat.h"

static struct nlist nlst[] = {
#define X_CCPU          0
        { "_ccpu" },
#define X_AVENRUN       1
        { "_avenrun" },
#define	X_HZ		2
	{ "_hz" },
#define	X_PHZ		3
	{ "_phz" },
        { "" }
};

int     kmem = -1;
int     mem = -1;
int     swap = -1;
int	naptime = 5;

int     die();
int     display();
int     suspend();
int	(*sigtstpdfl)();

double	ccpu;
int     dellave;

static	WINDOW *wload;			/* one line window for load average */

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
        nlist("/vmunix", nlst);
	if (nlst[X_CCPU].n_type == 0) {
		fprintf(stderr, "Couldn't namelist /vmunix.\n");
		exit(1);
	}
	kmemf = "/dev/kmem";
	kmem = open(kmemf, O_RDONLY);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	memf = "/dev/mem";
	mem = open(memf, O_RDONLY);
	if (mem < 0) {
		perror(memf);
		exit(1);
	}
	swapf = "/dev/drum";
	swap = open(swapf, O_RDONLY);
	if (swap < 0) {
		perror(swapf);
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
        lseek(kmem, nlst[X_CCPU].n_value, L_SET);
        read(kmem, &ccpu, sizeof (ccpu));
        lccpu = log(ccpu);
	hz = getw(nlst[X_HZ].n_value);
	phz = getw(nlst[X_PHZ].n_value);
	(*curcmd->c_init)();
	curcmd->c_flags |= CF_INIT;
        labels();

        known[0].k_uid = -1;
	known[0].k_name[0] = '\0';
        numknown = 1;
	procs[0].pid = -1;
	strcpy(procs[0].cmd, "<idle>");
	numprocs = 1;
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

display()
{
        register int i, j;

        /* Get the load average over the last minute. */
        lseek(kmem, nlst[X_AVENRUN].n_value, L_SET);
	read(kmem, avenrun, sizeof (avenrun));
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
	double	avenrun[3];

	lseek(kmem, nlst[X_AVENRUN].n_value, L_SET);
	read(kmem, avenrun, sizeof (avenrun));
	mvprintw(CMDLINE, 0, "%4.1f %4.1f %4.1f",
	    avenrun[0], avenrun[1], avenrun[2]);
	clrtoeol();
}

die()
{

        endwin();
        exit(0);
}

error(fmt, a1, a2, a3)
{

	mvprintw(CMDLINE, 0, fmt, a1, a2, a3);
	clrtoeol();
	refresh();
}
