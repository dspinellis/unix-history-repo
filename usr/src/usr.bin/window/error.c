#ifndef lint
static	char *sccsid = "@(#)error.c	3.8 84/04/08";
#endif

#include "defs.h"
#include "value.h"
#include "context.h"
#include "char.h"

struct ww *openwin();

extern int lineno;			/* line number in source file */

#define ERRLINES 10			/* number of lines in errwin */

/*VARARGS1*/
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
	register struct ww *w;

	if (cx.x_type != X_FILE) {
		if (terse)
			Ding();
		else {
			wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			wwputs("  ", cmdwin);
		}
		return;
	}
	if (cx.x_noerr)
		return;
	if ((w = cx.x_errwin) == 0) {
		char buf[512];

		if ((w = cx.x_errwin = openiwin(ERRLINES, buf)) == 0) {
			wwputs("Can't open error window.  ", cmdwin);
			cx.x_noerr = 1;
			return;
		}
	}
	if (more(w, 0) == 2) {
		cx.x_noerr = 1;
		return;
	}
}

err_end()
{
	char *malloc();

	filename = malloc(strlen(fn) + 1);
	strcpy(filename, fn);
	if (cx.x_errwin != 0) {
		if (!cx.x_noerr)
			waitnl(cx.x_errwin);
		closeiwin(cx.x_errwin);
		cx.x_errwin = 0;
	}
}
