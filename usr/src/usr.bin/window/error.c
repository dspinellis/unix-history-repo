#ifndef lint
static	char *sccsid = "@(#)error.c	3.4 83/12/07";
#endif

#include "defs.h"
#include "value.h"
#include "context.h"

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
	if (cx.x_noerrwin)
		return;
	if ((w = cx.x_errwin) == 0) {
		char buf[512];

		if ((w = cx.x_errwin = openiwin(ERRLINES, buf)) == 0) {
			(void) wwputs("Can't open error window.  ", cmdwin);
			cx.x_noerrwin = 1;
			return;
		}
	}
	if (w->ww_cur.r >= w->ww_w.b - 2) {
		waitnl(w);
		(void) wwputs("\033E", w);
	}
}

err_end()
{
	char *malloc();

	filename = malloc(strlen(fn) + 1);
	strcpy(filename, fn);
	if (cx.x_errwin != 0) {
		waitnl(cx.x_errwin);
		closeiwin(cx.x_errwin);
		cx.x_errwin = 0;
	}
}
