#ifndef lint
static	char *sccsid = "@(#)lcmd.c	3.6 83/08/25";
#endif

#include "defs.h"

int l_buffer();
int l_window();
int l_select();
int l_escape();
int l_label();
int l_terse();
int l_source();
int l_write();
int l_close();

struct lcmd {
	char *l_name;			/* name of command */
	int l_lmin;			/* minimum length to check */
	int l_lmax;			/* maximum length to check */
	int l_amin;			/* minimum argument */
	int l_amax;			/* maximum argument */
	int (*l_func)();		/* the function */
};
static struct lcmd lcmd[] = {
	"%",		1, 1, 0,  0, l_select,
	"buffer",	1, 0, 1,  1, l_buffer,
	"close",	1, 0, 0, -1, l_close,
	"escape",	1, 0, 1,  1, l_escape,
	"label",	1, 0, 2,  2, l_label,
	"source",	1, 0, 1,  1, l_source,
	"terse",	1, 0, 0,  1, l_terse,
	"window",	1, 0, 4,  5, l_window,
	"write",	2, 0, 2,  2, l_write,
	0
};

dosource(filename)
char *filename;
{
	register FILE *f;
	char buf[BUFSIZ];

	if ((f = fopen(filename, "r")) == 0)
		return -1;
	insource++;
	beginerror(filename);
	for (lineno = 1; fgets(buf, sizeof buf, f) != 0; lineno++)
		dolongcmd(buf);
	enderror();
	insource = 0;
	return 0;
}

dolongcmd(line)
char *line;
{
	register struct lcmd *lp;
	register len;

	makeargv(line);
	if (argc == 0)
		return;
	for (lp = lcmd; lp->l_name; lp++) {
		len = strlen(*argv);
		if (len < lp->l_lmin)
			continue;
		if (!strncmp(*argv, lp->l_name, lp->l_lmax ? lp->l_lmax : len))
			break;
	}
	if (lp->l_name) {
		if (lp->l_amin > argc - 1)
			error("Too few arguments.");
		else if (lp->l_amax >= 0 && lp->l_amax < argc - 1)
			error("Too many arguments.");
		else
			(*lp->l_func)();
	} else
		error("%s: Unknown command.", *argv);
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
	while (*p && *p != '\n' && (*p != '#' || escape || quote)
	       && pp < &argv[sizeof argv/sizeof *argv - 1]) {
		*pp++ = q;
		while (*p && *p != '\n') {
			if (escape) {
				switch (*p) {
				case 'n':
					*q++ = '\n';
					p++;
					break;
				case 'r':
					*q++ = '\r';
					p++;
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
				escape = 1;
				p++;
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
