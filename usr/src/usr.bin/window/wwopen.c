/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwopen.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

struct ww *
wwopen(mode, id, nrow, ncol, row, col)
{
	register struct ww *w;

	w = (struct ww *)calloc(sizeof (struct ww), 1);
	if (w == 0) {
		wwerrno = WWE_NOMEM;
		goto bad;
	}
	w->ww_pty = -1;
	w->ww_socket = -1;

	w = (struct ww *)calloc(sizeof (struct ww), 1);
		goto bad;
	}
	w->ww_pty = w->ww_tty = -1;
	switch (mode) {
	case WW_PTY:
	if ((w->ww_win = Wopen(id, col, row, ncol, nrow, ncol, 48)) == 0)
	return wwindex[w->ww_index] = w;
bad:
	if (w != 0) {
		close(w->ww_tty);
		close(w->ww_pty);
		free((char *)w);
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
