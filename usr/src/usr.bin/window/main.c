#ifndef lint
static	char *sccsid = "@(#)main.c	3.1 83/08/11";
#endif

#include "defs.h"

char escapec = CTRL(p);

#define next(a) (*++*(a) ? *(a) : (*++(a) ? *(a) : (char *)usage()))

/*ARGSUSED*/
main(argc, argv)
char **argv;
{
	register n;
	register char *p;
	char fast = 0;
	int imask;
	struct timezone timezone;

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
				(void) usage();
			}
		} else
			(void) usage();
	}
	if ((shell = getenv("SHELL")) == 0)
		shell = "/bin/csh";
	if (shellname = rindex(shell, '/'))
		shellname++;
	else
		shellname = shell;
	(void) gettimeofday(&starttime, &timezone);
	if (wwinit() < 0) {
		(void) fflush(stdout);
		(void) fprintf(stderr, "Can't do windows on this terminal.\n");
		exit(1);
	}
	if (debug) {
		wwnewtty.ww_tchars.t_quitc = wwoldtty.ww_tchars.t_quitc;
		(void) wwsettty(0, &wwnewtty);
	}

	if ((cmdwin = wwopen(WWO_REVERSE, 1, wwncol, 0, 0, 0)) == 0) {
		(void) wwflush();
		(void) fprintf(stderr, "Can't open command window.\r\n");
		goto bad;
	}
	if ((framewin = wwopen(WWO_GLASS, wwnrow, wwncol, 0, 0, 0)) == 0) {
		(void) wwflush();
		(void) fprintf(stderr, "Can't open frame window.\r\n");
		goto bad;
	}
	wwadd(framewin, &wwhead);

	curwin = cmdwin;
	wwupdate();
	wwflush();
	(void) signal(SIGCHLD, wwchild);
	if (!fast) {
		if (!terse)
			wwadd(cmdwin, &wwhead);
		if (doconfig() < 0)
			dodefault();
		if (selwin != 0) {
			curwin = selwin;
			/*
			Woncursor(selwin->ww_win, 0);
			*/
		}
		if (!terse) {
			wwdelete(cmdwin);
			reframe();
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
			wwcurtowin(curwin);
			wwupdate();
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
		 * Weird loop.  Copy the buffer to the pty
		 * and stopping on the escape character
		 * in a hopefully efficient way.
		 * Probably a good thing to make ibufc == 1 a special
		 * case.
		 */
		for (p = ibufp, n = ibufc;;) {
			if (--n < 0) {
				(void) write(curwin->ww_pty, ibufp, ibufc);
				ibufp = ibuf;
				ibufc = 0;
				break;
			} else if (*p++ == escapec) {
				if ((n = p - ibufp) > 1)
					(void) write(curwin->ww_pty,
						ibufp, n - 1);
				ibufp = p;
				ibufc -= n;
				curwin = cmdwin;
				break;
			}
		}
	}
	wwupdate();
	wwflush();
bad:
	wwend();
	return 0;
}

usage()
{
	(void) fprintf(stderr, "window: [-e escape] [-t] [-f]\n");
	exit(1);
	return 0;			/* for lint */
}
