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
static char sccsid[] = "@(#)error.c	3.16 (Berkeley) 6/6/90";
#endif /* not lint */

#include "defs.h"
#include "value.h"
#include "context.h"
#include "char.h"

#define ERRLINES 10			/* number of lines for errwin */

/*VARARGS1*/
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
	register struct context *x;
	register struct ww *w;

	for (x = &cx; x != 0 && x->x_type != X_FILE; x = x->x_link)
		;
	if (x == 0) {
		if (terse)
			wwbell();
		else {
			wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			wwputs("  ", cmdwin);
		}
		return;
	}
	if (x->x_noerr)
		return;
	if ((w = x->x_errwin) == 0) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", x->x_filename);
		if ((w = x->x_errwin = openiwin(ERRLINES, buf)) == 0) {
			wwputs("Can't open error window.  ", cmdwin);
			x->x_noerr = 1;
			return;
		}
	}
	if (more(w, 0) == 2) {
		x->x_noerr = 1;
		return;
	}
	wwprintf(w, "line %d: ", x->x_lineno);
	wwprintf(w, fmt, a, b, c, d, e, f, g, h);
	wwputc('\n', w);
}

err_end()
{
	if (cx.x_type == X_FILE && cx.x_errwin != 0) {
		if (!cx.x_noerr)
			waitnl(cx.x_errwin);
		closeiwin(cx.x_errwin);
		cx.x_errwin = 0;
	}
}
