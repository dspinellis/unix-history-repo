#ifndef lint
static	char *sccsid = "@(#)wwopen.c	1.1 83/07/12";
#endif

#include "ww.h"
#include <sys/types.h>
#include <sys/stat.h>

extern int _wwiflag;
struct ww *_wwhead = 0;

struct ww *
wwopen(nrow, ncol, row, col)
{
	register struct ww *w;

	w = (struct ww *)calloc(sizeof (struct ww), 1);
	if (w == 0)
		goto bad;
	if (wwgettchars(w) < 0)
		goto bad;
	if (wwgetpty(w) < 0)
		goto bad;
	if (!_wwiflag)
		wwinit();
	if ((w->ww_win = newwin(nrow, ncol, row, col)) == 0)
		goto bad;
	w->ww_col = col;
	w->ww_row = row;
	w->ww_ncol = ncol;
	w->ww_nrow = nrow;
	w->ww_next = _wwhead;
	w->ww_state = WW_INITIAL;
	_wwhead = w;
	return w;
bad:
	if (w != 0)
		cfree((char *)w);
	return 0;
}

wwgettchars(w)
	register struct ww *w;
{
	if (ioctl(0, TIOCGETP, &w->ww_sgttyb) < 0)
		return -1;
	if (ioctl(0, TIOCGETC, &w->ww_tchars) < 0)
		return -1;
	if (ioctl(0, TIOCGLTC, &w->ww_ltchars) < 0)
		return -1;
	if (ioctl(0, TIOCLGET, &w->ww_lmode) < 0)
		return -1;
	if (ioctl(0, TIOCGETD, &w->ww_ldisc) < 0)
		return -1;
	if (ioctl(0, TIOCGPGRP, &w->ww_pgrp) < 0)
		return -1;
	return 0;
}

wwgetpty(w)
	register struct ww *w;
{
	register char c;
	register char *line;
	register int i;
#define PTY "/dev/XtyXX"

	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;

		line = PTY;
		line[sizeof PTY - 6] = 'p';
		line[sizeof PTY - 3] = c;
		line[sizeof PTY - 2] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
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
	if (ioctl(w->ww_pty, TIOCSETP, &w->ww_sgttyb) < 0)
		goto bad;
	if (ioctl(w->ww_pty, TIOCSETC, &w->ww_tchars) < 0)
		goto bad;
	if (ioctl(w->ww_pty, TIOCSLTC, &w->ww_ltchars) < 0)
		goto bad;
	if (ioctl(w->ww_pty, TIOCLSET, &w->ww_lmode) < 0)
		goto bad;
	if (ioctl(w->ww_pty, TIOCSETD, &w->ww_ldisc) < 0)
		goto bad;
	if (ioctl(w->ww_pty, TIOCSPGRP, &w->ww_pgrp) < 0)
		goto bad;
	return 0;
bad:
	close(w->ww_pty);
	close(w->ww_tty);
	return -1;
}
