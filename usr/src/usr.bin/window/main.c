#ifndef lint
static	char *sccsid = "@(#)main.c	1.7 83/07/27";
#endif

#include "defs.h"

main()
{
	register n;
	register char *p;
	int wwchild();
	int imask;

	gettimeofday(&starttime, &timezone);
	if (wwinit() < 0) {
		fflush(stdout);
		fprintf("Can't do windows on this terminal.\n");
		exit(1);
	}
	if ((cmdwin = wwopen(WW_NONE, 0, 1, wwncol, 0, 0)) == 0) {
		fflush(stdout);
		fprintf(stderr, "Can't open command window.\r\n");
		goto bad;
	}
	wwsetcurwin(cmdwin);
	for (n = 0; n < wwncol; n++)			/* XXX */
		Waputc(0, WINVERSE|WBUF, cmdwin->ww_win);
	wwflush();
	(void) signal(SIGCHLD, wwchild);
	if (doconfig() < 0)
		dodefault();
	if (selwin != 0)
		wwsetcurwin(selwin);
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
			} else if (*p++ == ESCAPE) {
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
