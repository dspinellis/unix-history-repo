#ifndef lint
static	char *sccsid = "@(#)cmd5.c	1.3 83/07/22";
#endif

#include "defs.h"
#include <ctype.h>

struct ww *openwin();
struct ww *doopen();

static char *sourcefilename;
static int lineno;			/* current line number in source file */
static char *argv[100];			/* one line broken up into words */
static int argc;
static struct ww *errwin;		/* window for error reporting */
static int errlineno;			/* lineno in errwin */

dosource(filename)
char *filename;
{
	register FILE *f;
	register char **pp;
	char buf[BUFSIZ];
	int id, row, col, nrow, ncol;
	struct ww *w;

	if ((f = fopen(filename, "r")) == 0)
		return -1;
	sourcefilename = filename;
	for (lineno = 1; fgets(buf, sizeof buf, f) != 0; lineno++) {
		if (*buf == '#')
			continue;
		breakup(buf);
		if (argc == 0)
			continue;
		pp = argv;
		switch (**pp++) {
		case 'w':
			if ((id = findid()) < 0) {
				if (error("Too many windows.") < 0)
					goto bad;
				break;
			}
			if (argc < 5) {
				if (error("Syntax error.") < 0)
					goto bad;
				break;
			}
			if (**pp == '*')
				row = 0;
			else
				row = atoi(*pp);
			if (**++pp == '*')
				col = 0;
			else
				col = atoi(*pp);
			if (**++pp == '*')
				nrow = wwnrow - row;
			else
				nrow = atoi(*pp);
			if (**++pp == '*')
				ncol = wwncol - col;
			else
				ncol = atoi(*pp);
			w = doopen(id, nrow, ncol, row, col);
			if (w == 0) {
				if (error("Can't open window: row %d col %d, %d rows %d cols.", row, col, nrow, ncol) < 0)
					goto bad;
				break;
			}
			break;
		case '%':
			id = atoi(*pp);
			if (id < 1 || id > 9 || (w = wwfind(id)) == 0) {
				if (error("%d: No such window.", id) < 0)
					goto bad;
				break;
			}
			setselwin(w);
			break;
		default:
			if (error("%s: Unknown command.", *argv) < 0)
				goto bad;
		}
	}
	if (errwin != 0) {
		waitnl(errwin);
		closewin(errwin);
		errwin = 0;
	}
bad:
	fclose(f);
	return 0;
}

static
breakup(p)
register char *p;
{
	static char buf[BUFSIZ];
	register char *q = buf, **pp = argv;

	for (; *p && *p != '\n' && (*p == ' ' || *p == '\t'); p++)
		;
	while (*p && *p != '\n' && pp < &argv[sizeof argv/sizeof *argv - 1]) {
		*pp++ = q;
		if (isalnum(*p)) {
			while (*p && *p != '\n' && *p != ' ' && *p != '\t')
				*q++ = *p++;
		} else
			*q++ = *p++;
		*q++ = 0;
		for (; *p && *p != '\n' && (*p == ' ' || *p == '\t'); p++)
			;
	}
	*pp = 0;
	argc = pp - argv;
}

/*VARARGS1*/
static
error(fmt, a, b, c, d, e, f, g, h)
char *fmt;
{
#define ERRLINES 10
	if (errwin == 0) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", sourcefilename);
		if ((errwin = openwin(ERRLINES, buf)) == 0) {
			wwprintf(cmdwin, "Can't open error window.  ");
			return -1;
		}
		errlineno = 0;
	}
	if (errlineno++ > ERRLINES - 4) {
		waitnl(errwin);
		wwprintf(errwin, "\r\n");
		errlineno = 0;
	}
	wwprintf(errwin, "line %d: ", lineno);
	wwprintf(errwin, fmt, a, b, c, d, e, f, g, h);
	wwprintf(errwin, "\r\n");
	return 0;
}
