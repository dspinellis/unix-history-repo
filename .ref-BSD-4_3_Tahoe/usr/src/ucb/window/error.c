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
static char sccsid[] = "@(#)error.c	3.14 (Berkeley) 6/29/88";
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
