#ifndef lint
static	char *sccsid = "@(#)wwopen.c	1.6 83/07/29";
#endif

#include "ww.h"
#include <sys/types.h>
#include <sys/stat.h>

struct ww *
wwopen(mode, id, nrow, ncol, row, col)
{
	register struct ww *w = 0;

	w = (struct ww *)calloc(sizeof (struct ww), 1);
	if (w == 0)
		goto bad;
	w->ww_pty = w->ww_tty = -1;
	switch (mode) {
	case WW_PTY:
		if (wwgetpty(w) < 0)
			goto bad;
		if (wwsettty(w->ww_pty, &wwwintty) < 0)
			goto bad;
		break;
	case WW_SOCKET:
		break;
	case WW_NONE:
		break;
	default:
		goto bad;
	}
	if ((w->ww_win = Wopen(id, col, row, ncol, nrow, ncol, 48)) == 0)
		goto bad;
	Woncursor(w->ww_win, 0);		/* don't show cursor */
	w->ww_mode = mode;
	w->ww_ident = id;
	w->ww_w.col = w->ww_i.col = w->ww_o.col = col;
	w->ww_w.row = w->ww_i.row = w->ww_o.row = row;
	w->ww_w.ncol = w->ww_i.ncol = w->ww_o.ncol = ncol;
	w->ww_w.nrow = w->ww_i.nrow = w->ww_o.nrow = nrow;
	w->ww_next = wwhead;
	w->ww_state = WW_INITIAL;
	wwhead = w;
	return w;
bad:
	if (w != 0) {
		close(w->ww_tty);
		close(w->ww_pty);
		cfree((char *)w);
	}
	return 0;
}

wwgetpty(w)
	register struct ww *w;
{
	register char c;
	register char *line;
	register int i;
#define PTY "/dev/ptyXX"

	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;

		line = PTY;
		line[sizeof PTY - 6] = 'p';
		line[sizeof PTY - 3] = c;
		line[sizeof PTY - 2] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[sizeof PTY - 6] = 'p';
			line[sizeof PTY - 2] = "0123456789abcdef"[i];
			w->ww_pty = open(line, 2);
			if (w->ww_pty >= 0) {
				line[sizeof PTY - 6] = 't';
				w->ww_tty = open(line, 2);
				if (w->ww_tty >= 0)
					goto good;
				close(w->ww_pty);
			}
		}
	}
	return -1;
good:
	strcpy(w->ww_ttyname, line);
	return 0;
bad:
	close(w->ww_pty);
	close(w->ww_tty);
	return -1;
}
