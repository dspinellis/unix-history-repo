#ifndef lint
static	char *sccsid = "@(#)lcmd1.c	3.6 83/08/26";
#endif

#include "defs.h"

l_window()
{
	register char **pp = argv;
	int col, row, ncol, nrow, id, nline;

	if ((id = findid()) < 0)
		return;
	row = **++pp == '*' ? 1 : atoi(*pp);
	col = **++pp == '*' ? 0 : atoi(*pp);
	nrow = **++pp == '*' ? wwnrow - row : atoi(*pp);
	ncol = **++pp == '*' ? wwncol - col : atoi(*pp);
	nline = *++pp == 0 ? nbufline : atoi(*pp);
	(void) openwin(id, row, col, nrow, ncol, nline);
}

l_buffer()
{
	nbufline = atoi(argv[1]);
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

l_close()
{
	register i;
	register struct ww *w;
	char didit = 0;

	if (argc < 2) {
		c_close((struct ww *)0);
		return;
	}
	for (i = 1; i < argc; i++) {
		if ((w = idtowin(argv[i])) == 0)
			continue;
		closewin(w);
		didit++;
	}
	if (selwin == 0) {
		for (i = 0; i < NWINDOW && window[i] != 0; i++)
			;
		if (i < NWINDOW)
			setselwin(window[i]);
	}
	if (didit)
		reframe();
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
