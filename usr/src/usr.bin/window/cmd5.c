#ifndef lint
static	char *sccsid = "@(#)cmd5.c	1.5 83/07/28";
#endif

#include "defs.h"

struct ww *openwin();
struct ww *doopen();
struct ww *idtowin();

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
int s_label();
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
	"label",	1, 2, 2, s_label,
	0
};

dosource(filename)
char *filename;
{
	register FILE *f;
	char buf[BUFSIZ];

	if ((f = fopen(filename, "r")) == 0)
		return;
	beginerror(filename);
	for (lineno = 1; fgets(buf, sizeof buf, f) != 0; lineno++)
		doline(buf);
	enderror();
	return 0;
}

doline(line)
char *line;
{
	register struct scmd *sp;

	makeargv(line);
	if (argc == 0)
		return;
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
	struct ww *w;

	if ((w = idtowin(*argv + 1)) == 0)
		return;
	setselwin(w);
}

s_escape()
{
	setescape(argv[1]);
}

s_label()
{
	struct ww *w;

	if ((w = idtowin(argv[1])) == 0)
		return;
	setlabel(w, argv[2]);
}

struct ww *
idtowin(idstr)
char *idstr;
{
	int id;
	struct ww *w = 0;

	id = atoi(idstr);
	if (id < 1 || id > 9 || (w = wwfind(id)) == 0)
		error("%d: No such window.", id);
	return w;
}

makeargv(p)
register char *p;
{
	static char buf[BUFSIZ];
	register char *q = buf, **pp = argv;
	char quote = 0, escape = 0;
	int i;

	for (; *p == ' ' || *p == '\t'; p++)
		;
	while (*p && *p != '\n' && *p != '#'
	       && pp < &argv[sizeof argv/sizeof *argv - 1]) {
		*pp++ = q;
		while (*p && *p != '\n') {
			if (escape) {
				switch (*p) {
				case 'n':
					*q++ = '\n';
					break;
				case 'r':
					*q++ = '\r';
					break;
				case '0': case '1': case '2': case '3':
				case '4': case '5': case '6': case '7':
					*q = 0;
					for (i = 3; --i >= 0
					     && *p >= '0' && *p <= '9';)
						*q = *q << 3 | *p++ - '0';
					q++;
					break;
				default:
					*q++ = *p++;
					break;
				}
				escape = 0;
			} else if (*p == '\\') {
				escape == 1;
			} else if (quote) {
				if (*p == quote) {
					quote = 0;
					p++;
				} else
					*q++ = *p++;
			} else {
				if (*p == '"' || *p == '\'')
					quote = *p++;
				else if (*p == ' ' || *p == '\t')
					break;
				else
					*q++ = *p++;
			}
		}
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
	if (sourcefilename == 0) {
		wwprintf(cmdwin, fmt, a, b, c, d, e, f, g, h);
		wwprintf(cmdwin, ".  ");
		return;
	}
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

beginerror(filename)
char *filename;
{
	sourcefilename = filename;
}

enderror()
{
	if (errwin != 0) {
		waitnl(errwin);
		closewin(errwin);
		errwin = 0;
	} else
		baderrwin = 0;
	sourcefilename = 0;
}
