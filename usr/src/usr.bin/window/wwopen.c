/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwopen.c	3.27 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/types.h>
#include <sys/socket.h>
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
