#ifndef lint
static	char *sccsid = "@(#)main.c	1.4 83/07/19";
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
	if ((cmdwin = wwopen(1, 0, 1, WCols, 0, 0)) == 0) {
		fflush(stdout);
		fprintf(stderr, "Can't open command window.\r\n");
		goto bad;
	}
	wwsetcurrent(cmdwin);
	for (n = 0; n < WCols; n++)
		Waputc(0, WINVERSE|WBUF, cmdwin->ww_win);
	wwflush();
	(void) signal(SIGCHLD, wwchild);
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
			if ((ibufc = read(0, ibuf, sizeof ibuf)) < 0)
				ibufc = 0;
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
				wwsetcurrent(cmdwin);
				break;
			}
		}
	}
bad:
	wwend();
	return 0;
}
