/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwgets.c	3.15 (Berkeley) 6/6/90";
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

	w->ww_unctrl = 0;
	for (;;) {
		wwcurtowin(w);
		while ((c = wwgetc()) < 0)
			wwiomux();
#ifndef POSIX_TTY
		if (c == wwoldtty.ww_sgttyb.sg_erase)
#else
		if (c == wwoldtty.ww_termios.c_cc[VERASE])
#endif
		{
			if (p > buf)
				rub(*--p, w);
		} else
#ifndef POSIX_TTY
		if (c == wwoldtty.ww_sgttyb.sg_kill)
#else
		if (c == wwoldtty.ww_termios.c_cc[VKILL])
#endif
		{
			while (p > buf)
				rub(*--p, w);
		} else
#ifndef POSIX_TTY
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

static
rub(c, w)
struct ww *w;
{
	register i;

	for (i = strlen(unctrl(c)); --i >= 0;)
		(void) wwwrite(w, "\b \b", 3);
}
