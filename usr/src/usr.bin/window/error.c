#ifndef lint
static	char *sccsid = "@(#)error.c	3.3 83/11/22";
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
	if (cx.x_type != X_FILE) {
		if (terse)
			Ding();
		else {
			wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			wwputs("  ", cmdwin);
		}
		return;
	}
	if (cx.x_baderr)
		return;
	if (cx.x_errwin == 0) {
		char buf[512];

			cx.x_baderr = 1;
			return;
		}
		cx.x_errlineno = 0;
	}
	if (cx.x_errlineno++ > ERRLINES - 4) {
		waitnl(cx.x_errwin);
		cx.x_errlineno = 0;
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
