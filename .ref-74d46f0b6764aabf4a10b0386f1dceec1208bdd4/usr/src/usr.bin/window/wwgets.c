/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwgets.c	3.17 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "char.h"

wwgets(buf, n, w)
char *buf;
int n;
register struct ww *w;
{
	register char *p = buf;
	register char c;
	char uc = w->ww_unctrl;
	static void rub();

	w->ww_unctrl = 0;
	for (;;) {
		wwcurtowin(w);
		while ((c = wwgetc()) < 0)
			wwiomux();
#ifdef OLD_TTY
		if (c == wwoldtty.ww_sgttyb.sg_erase)
#else
		if (c == wwoldtty.ww_termios.c_cc[VERASE])
#endif
		{
			if (p > buf)
				rub(*--p, w);
		} else
#ifdef OLD_TTY
		if (c == wwoldtty.ww_sgttyb.sg_kill)
#else
		if (c == wwoldtty.ww_termios.c_cc[VKILL])
#endif
		{
			while (p > buf)
				rub(*--p, w);
		} else
#ifdef OLD_TTY
		if (c == wwoldtty.ww_ltchars.t_werasc)
#else
		if (c == wwoldtty.ww_termios.c_cc[VWERASE])
#endif
		{
			while (--p >= buf && (*p == ' ' || *p == '\t'))
				rub(*p, w);
			while (p >= buf && *p != ' ' && *p != '\t')
				rub(*p--, w);
			p++;
		} else if (c == '\r' || c == '\n') {
			break;
		} else {
			if (p >= buf + n - 1)
				wwputc(ctrl('g'), w);
			else
				wwputs(unctrl(*p++ = c), w);
		}
	}
	*p = 0;
	w->ww_unctrl = uc;
}

static void
rub(c, w)
struct ww *w;
{
	register i;

	for (i = strlen(unctrl(c)); --i >= 0;)
		(void) wwwrite(w, "\b \b", 3);
}
