#ifndef lint
static char sccsid[] = "@(#)main.c	3.30 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "defs.h"
#include <sys/signal.h>
#include <stdio.h>
#include "string.h"
#include "char.h"
#include "local.h"

#define next(a) (*++*(a) ? *(a) : (*++(a) ? *(a) : (char *)usage()))

/*ARGUSED*/
main(argc, argv)
char **argv;
{
	register char *p;
	char fflag = 0;
	char dflag = 0;
	char xflag = 0;
	char *cmd = 0;
	char tflag = 0;

	nbufline = NLINE;
	escapec = ESCAPEC;	
	if (p = rindex(*argv, '/'))
		p++;
	else
		p = *argv;
	debug = strcmp(p, "a.out") == 0;
	while (*++argv) {
		if (**argv == '-') {
			switch (*++*argv) {
			case 'f':
				fflag++;
				break;
			case 'c':
				if (cmd != 0) {
					(void) fprintf(stderr,
						"Only one -c allowed.\n");
					(void) usage();
				}
				cmd = next(argv);
				break;
			case 'e':
				setescape(next(argv));
				break;
			case 't':
				tflag++;
				break;
			case 'd':
				dflag++;
				break;
			case 'D':
				debug = !debug;
				break;
			case 'x':
				xflag++;
				break;
			default:
				usage();
			}
		} else
			usage();
	}
	if ((p = getenv("SHELL")) == 0)
		p = SHELL;
	if ((shellfile = str_cpy(p)) == 0)
		nomem();
	if (p = rindex(shellfile, '/'))
		p++;
	else
		p = shellfile;
	shell[0] = p;
	shell[1] = 0;
	gettimeofday(&starttime, &timezone);
	if (wwinit() < 0) {
		fflush(stdout);
		fprintf("Can't do windows on this terminal.\n");
		exit(1);
	}
	if (debug)
		wwnewtty.ww_tchars.t_quitc = wwoldtty.ww_tchars.t_quitc;
		wwsettty(0, &wwnewtty);
	}
	if (debug || xflag)
		(void) wwsettty(0, &wwnewtty, &wwoldtty);
	if ((cmdwin = wwopen(WW_NONE, 0, 1, wwncol, 0, 0)) == 0) {
		fflush(stdout);
		fprintf(stderr, "Can't open command window.\r\n");
		goto bad;
	}
	cmdwin->ww_mapnl = 1;
	cmdwin->ww_nointr = 1;
	cmdwin->ww_noupdate = 1;
	cmdwin->ww_unctrl = 1;
	if (terse)
		Whide(cmdwin->ww_win);
	wwsetcurwin(cmdwin);
	for (n = 0; n < wwncol; n++)			/* XXX */
		Waputc(0, WINVERSE|WBUF, cmdwin->ww_win);
	wwflush();
	(void) signal(SIGCHLD, wwchild);
	setvars();
		if (dflag || doconfig() < 0)
			dodefault();
	}

	mloop();

bad:
	wwend();
	return 0;
}

usage()
{
	fprintf(stderr, "window: [-e escape] [-t]\n");
	exit(1);
}
