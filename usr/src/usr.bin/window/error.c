#ifndef lint
static	char *sccsid = "@(#)error.c	3.3 83/11/22";
#endif

#include "defs.h"
#include "value.h"
#include "context.h"

#define ERRLINES 10			/* number of lines for errwin */

/*VARARGS1*/
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
	if (cx.x_type != X_FILE) {
		if (terse)
			wwbell();
		else {
			(void) wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			(void) wwputs("  ", cmdwin);
		}
		return;
	}
	if (cx.x_baderr)
		return;
	if (cx.x_errwin == 0) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", cx.x_filename);
		if ((cx.x_errwin = openiwin(ERRLINES, buf)) == 0) {
			(void) wwprintf(cmdwin, "Can't open error window.  ");
			cx.x_baderr = 1;
			return;
		}
		cx.x_errlineno = 0;
	}
	if (cx.x_errlineno++ > ERRLINES - 4) {
		waitnl(cx.x_errwin);
		cx.x_errlineno = 0;
	}
	if (cx.x_lineno != 0)
		(void) wwprintf(cx.x_errwin, "line %d: ", cx.x_lineno);
	(void) wwprintf(cx.x_errwin, fmt, a, b, c, d, e, f, g, h);
	(void) wwprintf(cx.x_errwin, "\n");
}

err_end()
{
	if (cx.x_errwin != 0) {
		waitnl(cx.x_errwin);
		closeiwin(cx.x_errwin);
		cx.x_errwin = 0;
	}
}
