#ifndef lint
static	char *sccsid = "@(#)cmd5.c	1.4 83/07/28";
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
static char baderrwin;			/* can't open errwin */

int s_window();
int s_select();
int s_escape();
struct scmd {
	char *s_name;			/* name of command */
	int s_len;			/* number of characters to check */
	int s_amin;			/* minimum argument */
	int s_amax;			/* maximum argument */
	int (*s_func)();		/* the function */
};
static struct scmd scmd[] = {
	"window",	1, 4, 4, s_window,
	"%",		1, 0, 0, s_select,
	"escape",	1, 1, 1, s_escape,
	0
};

dosource(filename)
char *filename;
{
	register FILE *f;
	char buf[BUFSIZ];
	register struct scmd *sp;

	if ((f = fopen(filename, "r")) == 0)
		return;
	sourcefilename = filename;
	for (lineno = 1; fgets(buf, sizeof buf, f) != 0; lineno++) {
		makeargv(buf);
		if (argc == 0)
			continue;
		for (sp = scmd; sp->s_name; sp++)
			if (sp->s_len > 0) {
				if (strncmp(*argv, sp->s_name, sp->s_len) == 0)
					break;
			} else
				if (strncmp(*argv, sp->s_name) == 0)
					break;
		if (sp->s_name) {
			if (sp->s_amin > argc - 1)
				error("Too few arguments.");
			else if (sp->s_amax < argc - 1)
				error("Too many arguments.");
			else
				(*sp->s_func)();
		} else
			error("%s: Unknown command.", *argv);
	}
	enderror();
	return 0;
}

s_window()
{
	register char **pp = argv;
	register struct ww *w;
	int col, row, ncol, nrow, id;

	if ((id = findid()) < 0) {
		error("Too many windows.");
		return;
	}
	if (**++pp == '*')
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
	if (w == 0)
		error("Can't open window: row %d col %d, %d rows %d cols.",
			row, col, nrow, ncol);
}

s_select()
{
	register int id;
	register struct ww *w;

	id = atoi(*argv + 1);
	if (id < 1 || id > 9 || (w = wwfind(id)) == 0)
		error("%d: No such window.", id);
	else
		setselwin(w);
}

s_escape()
{
	setescape(argv[1]);
}

makeargv(p)
register char *p;
{
	static char buf[BUFSIZ];
	register char *q = buf, **pp = argv;

	for (; *p == ' ' || *p == '\t'; p++)
		;
	while (*p && *p != '\n' && *p != '#'
	       && pp < &argv[sizeof argv/sizeof *argv - 1]) {
		*pp++ = q;
		while (*p && *p != '\n' && *p != ' ' && *p != '\t')
			*q++ = *p++;
		*q++ = 0;
		for (; *p == ' ' || *p == '\t'; p++)
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
	if (errwin == 0 && !baderrwin) {
		char buf[512];

		(void) sprintf(buf, "Errors from %s", sourcefilename);
		if ((errwin = openwin(ERRLINES, buf)) == 0) {
			wwprintf(cmdwin, "Can't open error window.  ");
			baderrwin++;
			return;
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
}

enderror()
{
	if (errwin != 0) {
		waitnl(errwin);
		closewin(errwin);
		errwin = 0;
	} else
		baderrwin = 0;
}
