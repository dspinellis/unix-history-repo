/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwgets.c	3.12 (Berkeley) %G%";
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
		if (c == wwoldtty.ww_sgttyb.sg_erase) {
			if (p > buf)
				rub(*--p, w);
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
