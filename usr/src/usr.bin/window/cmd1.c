#ifndef lint
static	char *sccsid = "@(#)cmd1.c	1.1 83/07/18";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

dowindow()
{
	int col, row, xcol, xrow;
	register struct ww *w;
	int id;
	char ids[10];

	for (id = 1; id < 10; id++)
		ids[id] = 0;
	for (w = wwhead; w; w = w->ww_next)
		ids[w->ww_ident] = 1;
	for (id = 1; id < 10 && ids[id]; id++)
		;
	if (id == 10) {
		wwputs("Too many windows.  ", cmdwin);
		return;
	}
	wwputs("Upper left corner: ", cmdwin);
	col = 0;
	row = 1;
	for (;;) {
		wwsetcursor(row, col);
		while (bpeekc() < 0) {
			wwflush();
			bread();
		}
		switch (getpos(&row, &col, 0, 0)) {
		case -1:
			WBoxActive = 0;
			goto out;
		case 1:
			break;
		case 0:
			continue;
		}
		break;
	}
	wwprintf(cmdwin, "%d %d.  Upper left corner: ", col, row);
	xcol = col + 1;
	xrow = row + 1;
	for (;;) {
		Wbox(col, row, xcol - col + 1, xrow - row + 1);
		wwsetcursor(xrow, xcol);
		wwflush();
		while (bpeekc() < 0) {
			wwflush();
			bread();
		}
		switch (getpos(&xrow, &xcol, row + 1, col + 1)) {
		case -1:
			WBoxActive = 0;
			goto out;
		case 1:
			break;
		case 0:
			continue;
		}
		break;
	}
	WBoxActive = 0;
	wwprintf(cmdwin, "%d %d.  ", xcol, xrow);
	if ((w = wwopen(WW_PTY, id, xrow-row+1, xcol-col+1, row, col)) == 0) {
		wwprintf(cmdwin, "Can't open window.  ");
		return;
	}
	wwframe(w);
	labelwin(w, 0);
	/*
	reframe();
	*/
	wwsetcursor(WCurRow(w->ww_win), WCurCol(w->ww_win));
	wwflush();
	switch (wwfork(w)) {
	case -1:
		wwprintf(cmdwin, "Can't fork.  ");
		wwclose(w);
		return;
	case 0:
		execl("/bin/csh", "csh", 0);
		perror("exec(csh)");
		exit(1);
	}
	if (selwin == 0)
		setselwin(w);
	else
		wwsetcurrent(cmdwin);
out:
	wwputs("\r\n", cmdwin);
}

getpos(row, col, minrow, mincol)
register int *row, *col, minrow, mincol;
{
	static int scount = 0;
	int count;
	char c;

	while ((c = bgetc()) >= 0) {
		switch (c) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			scount = scount * 10 + c - '0';
			continue;
		}
		count = scount ? scount : 1;
		scount = 0;
		switch (c) {
		case 'h':
			if ((*col -= count) < mincol)
				*col = mincol;
			break;
		case 'H':
			*col = mincol;
			break;
		case 'l':
			if ((*col += count) >= WCols)
				*col = WCols - 1;
			break;
		case 'L':
			*col = WCols - 1;
			break;
		case 'j':
			if ((*row += count) >= WRows)
				*row = WRows - 1;
			break;
		case 'J':
			*row = WRows - 1;
			break;
		case 'k':
			if ((*row -= count) < minrow)
				*row = minrow;
			break;
		case 'K':
			*row = minrow;
			break;
		case CTRL([):
			return -1;
		case '\r':
			return 1;
		default:
			Ding();
		}
	}
	return 0;
}

/*
reframe()
{
	register struct ww *w;

	for (w = wwhead; w; w = w->ww_next) {
		wwunframe(w);
		wwframe(w);
		labelwin(w, selwin == w ? WINVERSE : 0);
	}
}
*/
