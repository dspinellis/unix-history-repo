#ifndef lint
static	char *sccsid = "@(#)main.c	2.1 83/07/30";
#endif

#include "defs.h"

char escapec = CTRL(p);

#define next(a) (*++*(a) ? *(a) : (*++(a) ? *(a) : (char *)usage()))

/*ARGUSED*/
main(argc, argv)
char **argv;
{
	register n;
	register char *p;
	char fast = 0;
	int wwchild();
	int imask;
	char *rindex();
	char *getenv();

	if (p = rindex(*argv, '/'))
		p++;
	else
		p = *argv;
	debug = strcmp(p, "a.out") == 0;
	while (*++argv) {
		if (**argv == '-') {
			switch (*++*argv) {
			case 'f':
				fast++;
				break;
			case 'e':
				setescape(next(argv));
				break;
			case 't':
				terse++;
				break;
			case 'd':
				debug++;
				break;
			default:
				usage();
			}
		} else
			usage();
	}
	if ((shell = getenv("SHELL")) == 0)
		shell = "/bin/csh";
	if (shellname = rindex(shell, '/'))
		shellname++;
	else
		shellname = shell;
	gettimeofday(&starttime, &timezone);
	if (wwinit() < 0) {
		fflush(stdout);
		fprintf("Can't do windows on this terminal.\n");
		exit(1);
	}
	if (debug) {
		wwnewtty.ww_tchars.t_quitc = wwoldtty.ww_tchars.t_quitc;
		wwsettty(0, &wwnewtty);
	}
	if ((cmdwin = wwopen(WW_NONE, 0, 1, wwncol, 0, 0)) == 0) {
		fflush(stdout);
		fprintf(stderr, "Can't open command window.\r\n");
		goto bad;
	}
	if (terse)
		Whide(cmdwin->ww_win);
	wwsetcurwin(cmdwin);
	for (n = 0; n < wwncol; n++)			/* XXX */
		Waputc(0, WINVERSE|WBUF, cmdwin->ww_win);
	wwflush();
	(void) signal(SIGCHLD, wwchild);
	if (!fast) {
		if (doconfig() < 0)
			dodefault();
		if (selwin != 0) {
			wwsetcurwin(selwin);
			Woncursor(selwin->ww_win, 0);
		}
	}
	while (!quit) {
		if (curwin == cmdwin) {
			docmd();
			continue;
		}
		/*
		 * Loop until we get some keyboard input.
		 */
		while (ibufc == 0) {
			wwsetcursor(WCurRow(curwin->ww_win),
				WCurCol(curwin->ww_win));
			wwflush();
			imask = 1 << 0;
			while (wwforce(&imask) < 0)
				;
			if ((imask & 1 << 0) == 0)
				continue;
			/* NOTE: ibufc == 0 */
			ibufp = ibuf;
			if ((ibufc = read(0, ibuf, sizeof ibuf)) < 0) {
				ibufc = 0;
				nreade++;
			} else if (ibufc == 0)
				nreadz++;
			else
				nreadc += ibufc;
			nread++;
		}
		/*
		 * Weird loop.  Copy the buffer to the pty stopping
		 * on the escape character in a hopefully efficient
		 * way.
		 * Probably a good thing to make ibufc == 1 a special
		 * case.
		 */
		for (p = ibufp, n = ibufc;;) {
			if (--n < 0) {
				write(curwin->ww_pty, ibufp, ibufc);
				ibufp = ibuf;
				ibufc = 0;
				break;
			} else if (*p++ == escapec) {
				if ((n = p - ibufp) > 1)
					write(curwin->ww_pty, ibufp, n - 1);
				ibufp = p;
				ibufc -= n;
				wwsetcurwin(cmdwin);
				break;
			}
		}
	}
bad:
	wwend();
	return 0;
}

usage()
{
	fprintf(stderr, "window: [-e escape] [-t]\n");
	exit(1);
}
