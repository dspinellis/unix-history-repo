#ifndef lint
static	char *sccsid = "@(#)main.c	1.3 83/07/18";
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
		while (ibufc == 0) {
			wwsetcursor(WCurRow(curwin->ww_win),
				WCurCol(curwin->ww_win));
			wwflush();
			imask = 1<<0;
			wwforce(&imask);
			if ((imask & 1<<0) == 0)
				continue;
			if (ibufc == 0)
				ibufp = ibuf;
			p = ibufp + ibufc;
			n = read(0, p, ibuf + sizeof ibuf - p);
			if (n > 0)
				ibufc = n;
		}
		for (p = ibufp, n = ibufc; n-- > 0 && *p != ESCAPE; p++)
			;
		if ((n = p - ibufp) > 0) {
			write(curwin->ww_pty, ibufp, n);
			ibufp = p;
			ibufc -= n;
		}
		if (*p == ESCAPE) {
			ibufp++;
			ibufc--;
			wwsetcurrent(cmdwin);
		}
	}
bad:
	wwend();
	return 0;
}
