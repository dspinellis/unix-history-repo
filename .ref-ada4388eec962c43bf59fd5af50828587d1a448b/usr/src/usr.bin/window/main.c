/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include "defs.h"
#include <paths.h>
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
		p = _PATH_BSHELL;
	if ((default_shellfile = str_cpy(p)) == 0) {
		(void) fprintf(stderr, "Out of memory.\n");
		exit(1);
	}
	if (p = rindex(default_shellfile, '/'))
		p++;
	else
		p = default_shellfile;
	default_shell[0] = p;
	default_shell[1] = 0;
	default_nline = NLINE;
	default_smooth = 1;
	gettimeofday(&starttime, &timezone);
	if (wwinit() < 0) {
		fflush(stdout);
		fprintf("Can't do windows on this terminal.\n");
		exit(1);
	}

#ifdef OLD_TTY
	if (debug)
		wwnewtty.ww_tchars.t_quitc = wwoldtty.ww_tchars.t_quitc;
		wwsettty(0, &wwnewtty);
	}
#else
	if (debug) {
		wwnewtty.ww_termios.c_cc[VQUIT] =
			wwoldtty.ww_termios.c_cc[VQUIT];
		wwnewtty.ww_termios.c_lflag |= ISIG;
	}
	if (xflag) {
		wwnewtty.ww_termios.c_cc[VSTOP] =
			wwoldtty.ww_termios.c_cc[VSTOP];
		wwnewtty.ww_termios.c_cc[VSTART] =
			wwoldtty.ww_termios.c_cc[VSTART];
		wwnewtty.ww_termios.c_iflag |= IXON;
	}
#endif
	if (debug || xflag)
		(void) wwsettty(0, &wwnewtty);
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
	setvars();
		if (dflag || doconfig() < 0)
			dodefault();
	if (selwin != 0)
		setcmd(0);

	mloop();

bad:
	wwend(1);
	return 0;
}

usage()
{
	fprintf(stderr, "window: [-e escape] [-t]\n");
	exit(1);
}
