#ifndef lint
static	char *sccsid = "@(#)wwgets.c	3.2 83/08/26";
#endif

#include "defs.h"

char *ibufp = ibuf;

bread()
{
	register n;
	register char *p;
	int imask;

	while (ibufc == 0) {
		wwupdate();
		wwflush();
		while (imask = 1, wwforce(&imask) < 0)
			;
		if ((imask & 1) == 0)
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
		wwcurtowin(w);
		while ((c = bgetc()) < 0)
			bread();
		if (c == wwoldtty.ww_sgttyb.sg_erase) {
			if (p > buf)
				rub(*--p, w);
			else
				wwbell();
		} else if (c == wwoldtty.ww_sgttyb.sg_kill) {
			while (p > buf)
				rub(*--p, w);
		} else if (c == wwoldtty.ww_ltchars.t_werasc) {
			while (--p >= buf && (*p == ' ' || *p == '\t'))
				rub(*p, w);
			while (p >= buf && *p != ' ' && *p != '\t')
				rub(*p--, w);
			p++;
		} else if (c == '\r' || c == '\n') {
			break;
		} else {
			if (p >= buf + n - 1)
				wwbell();
			else
				(void) wwputs(unctrl(*p++ = c), w);
		}
	}
	*p = 0;
}

rub(c, w)
struct ww *w;
{
	register i;

	for (i = strlen(unctrl(c)); --i >= 0;)
		(void) wwputs("\b \b", w);
}
