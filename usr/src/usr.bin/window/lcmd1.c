#ifndef lint
static	char *sccsid = "@(#)lcmd1.c	3.2 83/08/11";
#endif

#include "defs.h"

struct ww *openwin();
struct ww *idtowin();

l_window()
{
	register char **pp = argv;
	register struct ww *w;
	int col, row, ncol, nrow, id;

	if ((id = findid()) < 0) {
		error("Too many windows.");
		return;
	}
	if (**++pp == '*')
		row = 1;
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
	w = openwin(id, nrow, ncol, row, col);
	if (w == 0)
		error("Can't open window: row %d col %d, %d rows %d cols.",
			row, col, nrow, ncol);
}

l_select()
{
	struct ww *w;

	if ((w = idtowin(*argv + 1)) == 0)
		return;
	setselwin(w);
}

l_escape()
{
	setescape(argv[1]);
}

l_label()
{
	struct ww *w;

	if ((w = idtowin(argv[1])) == 0)
		return;
	if (setlabel(w, argv[2]) < 0)
		error("Out of memory.");
	reframe();
}

l_terse()
{
	char oldterse = terse;

	if (argc < 2)
		terse = 1;
	else if (strcmp(argv[1], "off") == 0)
		terse = 0;
	else
		terse = 1;
	if (terse && !oldterse)
		wwdelete(cmdwin);
	else if (!terse && oldterse)
		wwadd(cmdwin, &wwhead);
	reframe();
}

l_source()
{
	if (insource) {
		error("Recursive source.");
		return;
	}
	if (dosource(argv[1]) < 0)
		error("Can't open %s.", argv[1]);
}

l_write()
{
	struct ww *w;

	if ((w = idtowin(argv[1])) == 0)
		return;
	(void) write(w->ww_pty, argv[2], strlen(argv[2]));
}

struct ww *
idtowin(idstr)
char *idstr;
{
	register id;
	struct ww *w;

	id = atoi(idstr) - 1;
	if (id < 0 || id >= NWINDOW || (w = window[id]) == 0) {
		error("%d: No such window.", id + 1);
		return 0;
	}
	return w;
}
