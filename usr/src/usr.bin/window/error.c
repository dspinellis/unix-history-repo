#ifndef lint
static	char *sccsid = "@(#)error.c	1.2 83/07/29";
#endif

#include "defs.h"

struct ww *openwin();

extern int lineno;			/* line number in source file */

static char *filename;			/* source file name */
static struct ww *errwin;		/* window for error reporting */
static int errlineno;			/* lineno in errwin */
static char baderror;			/* can't open the error window */

#define ERRLINES 10			/* number of lines in errwin */

/*VARARGS1*/
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
	if (filename == 0) {
		if (terse)
			Ding();
		else {
			wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			wwputs("  ", cmdwin);
		}
		return;
	}
	if (baderror)
		return;
	if (errwin == 0) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", filename);
		if ((errwin = openwin(ERRLINES, buf)) == 0) {
			wwprintf(cmdwin, "Can't open error window.  ");
			baderror++;
			return;
		}
		errlineno = 0;
	}
	if (errlineno++ > ERRLINES - 4) {
		waitnl(errwin);
		errlineno = 0;
	}
	if (lineno != 0)
		wwprintf(errwin, "line %d: ", lineno);
	wwprintf(errwin, fmt, a, b, c, d, e, f, g, h);
	wwprintf(errwin, "\r\n");
}

beginerror(fn)
char *fn;
{
	char *malloc();

	filename = malloc(strlen(fn) + 1);
	strcpy(filename, fn);
}

enderror()
{
	if (errwin != 0) {
		waitnl(errwin);
		closewin(errwin);
		errwin = 0;
	}
	baderror = 0;
	free(filename);
	filename = 0;
}
