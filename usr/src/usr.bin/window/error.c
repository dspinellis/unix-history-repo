#ifndef lint
static	char *sccsid = "@(#)error.c	3.6 84/01/13";
#endif

#include "defs.h"
#include "value.h"
#include "context.h"

#define ERRLINES 10			/* number of lines for errwin */

/*VARARGS1*/
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
	register struct ww *w;

	if (cx.x_type != X_FILE) {
		if (terse)
			wwbell();
		else {
			(void) wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			(void) wwputs("  ", cmdwin);
		}
		return;
	}
	if (cx.x_noerr)
		return;
	if ((w = cx.x_errwin) == 0) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", cx.x_filename);
		if ((w = cx.x_errwin = openiwin(ERRLINES, buf)) == 0) {
			(void) wwputs("Can't open error window.  ", cmdwin);
			cx.x_noerr = 1;
			return;
		}
	}
	if (more(w, 0) == 2) {
		cx.x_noerr = 1;
		return;
	}
	(void) wwprintf(w, "line %d: ", cx.x_lineno);
	(void) wwprintf(w, fmt, a, b, c, d, e, f, g, h);
	(void) wwputc('\n', w);
}

err_end()
{
	if (cx.x_errwin != 0) {
		if (!cx.x_noerr)
			waitnl(cx.x_errwin);
		closeiwin(cx.x_errwin);
		cx.x_errwin = 0;
	}
}
