#ifndef lint
static	char *sccsid = "@(#)error.c	3.2 83/08/16";
#endif

#include "defs.h"

static char *filename;			/* source file name */
static struct ww *errwin;		/* window for error reporting */
static int errlineno;			/* lineno in errwin */
static char baderror;			/* can't open the error window */

#define ERRLINES 10			/* number of lines for errwin */

/*VARARGS1*/
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
	if (filename == 0) {
		if (terse)
			wwbell();
		else {
			(void) wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
			(void) wwputs("  ", cmdwin);
		}
		return;
	}
	if (baderror)
		return;
	if (errwin == 0) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", filename);
		if ((errwin = openiwin(ERRLINES, buf)) == 0) {
			(void) wwprintf(cmdwin, "Can't open error window.  ");
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
		(void) wwprintf(errwin, "line %d: ", lineno);
	(void) wwprintf(errwin, fmt, a, b, c, d, e, f, g, h);
	(void) wwprintf(errwin, "\n");
}

beginerror(fn)
char *fn;
{
	filename = malloc((unsigned) strlen(fn) + 1);
	(void) strcpy(filename, fn);
}

enderror()
{
	if (errwin != 0) {
		waitnl(errwin);
		closeiwin(errwin);
		errwin = 0;
	}
	baderror = 0;
	free(filename);
	filename = 0;
}
