#ifndef lint
static	char *sccsid = "@(#)main.c	3.18 84/04/05";
#endif

#include "defs.h"
#include <sys/signal.h>
#include <stdio.h>

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
	nbufline = 48;				/* compatible */
	escapec = CTRL(p);	
	if ((shell = getenv("SHELL")) == 0)
		shell = "/bin/csh";
	if (shellname = rindex(shell, '/'))
		shellname++;
	else
		shellname = shell;
#ifndef O_4_1A
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
		(void) wwsettty(0, &wwnewtty);
	if ((cmdwin = wwopen(WW_NONE, 0, 1, wwncol, 0, 0)) == 0) {
		fflush(stdout);
		fprintf(stderr, "Can't open command window.\r\n");
		goto bad;
	}
	cmdwin->ww_nointr = 1;
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
