#ifndef lint
static	char *sccsid = "@(#)wwgets.c	1.5 83/07/28";
#endif

#include "defs.h"

char *ibufp = ibuf;

bread()
{
	register n;
	register char *p;
	int imask;

	while (ibufc == 0) {
		wwflush();
		imask = 1 << 0;
		while (wwforce(&imask) < 0)
			;
		if ((imask & 1 << 0) == 0)
			continue;
		if (ibufc == 0) {
			p = ibufp = ibuf;
			n = sizeof ibuf;
		} else {
			p = ibufp + ibufc;
			n = (ibuf + sizeof ibuf) - p;
		}
		if ((n = read(0, p, n)) > 0) {
			ibufc += n;
			nreadc += n;
		} else if (n == 0)
			nreadz++;
		else
			nreade++;
		nread++;
	}
}

bgets(buf, n, w)
char *buf;
int n;
register struct ww *w;
{
	register char *p = buf;
	register char c;

	for (;;) {
		wwsetcursor(WCurRow(w->ww_win), WCurCol(w->ww_win));
		while ((c = bpeekc()) < 0)
			bread();
		switch (c) {
		case '\b':
			if (p > buf) {
				wwputs("\b \b", w);
				p--;
				if (ISCTRL(*p))
					wwputs("\b \b", w);
			} else
				Ding();
			break;
		case '\r':
		case '\n':
			*p = 0;
			return;
		default:
			if (p >= buf + n - 1)
				Ding();
			else {
				*p++ = c;
				wwputs(unctrl(*p), w);
			}
		}
	}
}
