#ifndef lint
static	char *sccsid = "@(#)cmd1.c	2.1.1.1 83/08/09";
#endif

#include "defs.h"

c_window()
{
	int col, row, xcol, xrow;
	int id;

	if ((id = findid()) < 0) {
		error("Too many windows.");
		return;
	}
	if (!terse)
		(void) wwputs("Upper left corner: ", cmdwin);
	col = 0;
	row = 1;
	for (;;) {
		wwsetcursor(row, col);
		while (bpeekc() < 0)
			bread();
		switch (getpos(&row, &col, 1, 0)) {
		case -1:
			/*
			WBoxActive = 0;
			*/
			if (!terse)
				(void) wwputs("\r\nCancelled.  ", cmdwin);
			return;
		case 1:
			break;
		case 0:
			continue;
		}
		break;
	}
	if (!terse)
		(void) wwputs("\r\nLower right corner: ", cmdwin);
	xcol = col;
	xrow = row;
	for (;;) {
		/*
		Wbox(col, row, xcol - col + 1, xrow - row + 1);
		*/
		wwsetcursor(xrow, xcol);
		wwflush();
		while (bpeekc() < 0)
			bread();
		switch (getpos(&xrow, &xcol, row, col)) {
		case -1:
			/*
			WBoxActive = 0;
			*/
			if (!terse)
				(void) wwputs("\r\nCancelled.  ", cmdwin);
			return;
		case 1:
			break;
		case 0:
			continue;
		}
		break;
	}
	/*
	WBoxActive = 0;
	*/
	if (!terse)
		(void) wwputs("\r\n", cmdwin);
	wwcurtowin(cmdwin);
	if (openwin(id, xrow-row+1, xcol-col+1, row, col) == 0)
		error("Can't open window.");
}

findid()
{
	register i;

	for (i = 0; i < NWINDOW && window[i] != 0; i++)
		;
	return i < NWINDOW ? i : -1;
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
			if ((*col += count) >= wwncol)
				*col = wwncol - 1;
			break;
		case 'L':
			*col = wwncol - 1;
			break;
		case 'j':
			if ((*row += count) >= wwnrow)
				*row = wwnrow - 1;
			break;
		case 'J':
			*row = wwnrow - 1;
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
			if (!terse)
				(void) wwputs("\r\nType [hjklHJKL] to move, return to enter position, escape to cancel.", cmdwin);
			wwbell();
		}
	}
	return 0;
}

struct ww *
openwin(id, nrow, ncol, row, col)
int id, nrow, ncol, row, col;
{
	register struct ww *w;

	if (row <= 0)
		return 0;
	if (id < 0 && (id = findid()) < 0)
		return 0;
	if ((w = wwopen(WWO_PTY, nrow, ncol, row, col, 0)) == 0)
		return 0;
	w->ww_id = id;
	window[id] = w;
	w->ww_hasframe = 1;
	wwadd(w, (selwin ? selwin : wwhead.ww_back));
	setselwin(w);
	/*
	wwupdate();
	wwflush();
	*/
	switch (wwfork(w)) {
	case -1:
		c_close(w);
		return 0;
	case 0:
		execl(shell, shellname, 0);
		perror(shell);
		exit(1);
	}
	return w;
}

reframe()
{
	register struct ww *w;

	wwunframe(framewin);
	for (w = wwhead.ww_back; w != &wwhead; w = w->ww_back)
		if (w->ww_hasframe)
			wwframe(w, framewin);
	for (w = wwhead.ww_back; w != &wwhead; w = w->ww_back)
		if (w->ww_hasframe)
			labelwin(w);
}
