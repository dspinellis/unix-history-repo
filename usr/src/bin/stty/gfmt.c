/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)gfmt.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <stdio.h>
#include <string.h>

#include "stty.h"
#include "extern.h"

static void gerr __P((char *));

void
gprint(tp, wp, ldisc)
	struct termios *tp;
	struct winsize *wp;
	int ldisc;
{
	register struct cchar *cp;

	(void)printf("gfmt1:cflag=%x:iflag=%x:lflag=%x:oflag=%x:",
	    tp->c_cflag, tp->c_iflag, tp->c_lflag, tp->c_oflag);
	for (cp = cchars1; cp->name; ++cp)
		(void)printf("%s=%x:", cp->name, tp->c_cc[cp->sub]);
	(void)printf("ispeed=%d:ospeed=%d\n", cfgetispeed(tp), cfgetospeed(tp));
}

void
gread(tp, s) 
	register struct termios *tp;
	char *s;
{
	register char *ep, *p;
	long tmp;

#define	CHK(s)	(*p == s[0] && !strcmp(p, s))
	if (!(s = strchr(s, ':')))
		gerr(NULL);
	for (++s; s;) {
		p = strsep(&s, ":\0");
		if (!p || !*p)
			break;
		if (!(ep = strchr(p, '=')))
			gerr(p);
		*ep++ = '\0';
		(void)sscanf(ep, "%lx", &tmp);
		if (CHK("cflag")) {
			tp->c_cflag = tmp;
			continue;
		}
		if (CHK("discard")) {
			tp->c_cc[VDISCARD] = tmp;
			continue;
		}
		if (CHK("dsusp")) {
			tp->c_cc[VDSUSP] = tmp;
			continue;
		}
		if (CHK("eof")) {
			tp->c_cc[VEOF] = tmp;
			continue;
		}
		if (CHK("eol")) {
			tp->c_cc[VEOL] = tmp;
			continue;
		}
		if (CHK("eol2")) {
			tp->c_cc[VEOL2] = tmp;
			continue;
		}
		if (CHK("erase")) {
			tp->c_cc[VERASE] = tmp;
			continue;
		}
		if (CHK("iflag")) {
			tp->c_iflag = tmp;
			continue;
		}
		if (CHK("intr")) {
			tp->c_cc[VINTR] = tmp;
			continue;
		}
		if (CHK("ispeed")) {
			(void)sscanf(ep, "%ld", &tmp);
			tp->c_ispeed = tmp;
			continue;
		}
		if (CHK("kill")) {
			tp->c_cc[VKILL] = tmp;
			continue;
		}
		if (CHK("lflag")) {
			tp->c_lflag = tmp;
			continue;
		}
		if (CHK("lnext")) {
			tp->c_cc[VLNEXT] = tmp;
			continue;
		}
		if (CHK("oflag")) {
			tp->c_oflag = tmp;
			continue;
		}
		if (CHK("ospeed")) {
			(void)sscanf(ep, "%ld", &tmp);
			tp->c_ospeed = tmp;
			continue;
		}
		if (CHK("quit")) {
			tp->c_cc[VQUIT] = tmp;
			continue;
		}
		if (CHK("reprint")) {
			tp->c_cc[VREPRINT] = tmp;
			continue;
		}
		if (CHK("start")) {
			tp->c_cc[VSTART] = tmp;
			continue;
		}
		if (CHK("status")) {
			tp->c_cc[VSTATUS] = tmp;
			continue;
		}
		if (CHK("stop")) {
			tp->c_cc[VSTOP] = tmp;
			continue;
		}
		if (CHK("susp")) {
			tp->c_cc[VSUSP] = tmp;
			continue;
		}
		if (CHK("vmin")) {
			(void)sscanf(ep, "%ld", &tmp);
			tp->c_cc[VMIN] = tmp;
			continue;
		}
		if (CHK("vtime")) {
			(void)sscanf(ep, "%ld", &tmp);
			tp->c_cc[VTIME] = tmp;
			continue;
		}
		if (CHK("werase")) {
			tp->c_cc[VWERASE] = tmp;
			continue;
		}
		gerr(p);
	}
}

static void
gerr(s)
	char *s;
{
	if (s)
		errx(1, "illegal gfmt1 option -- %s", s);
	else
		errx(1, "illegal gfmt1 option");
}
