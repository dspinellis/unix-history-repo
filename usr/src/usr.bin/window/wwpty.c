#ifndef lint
static	char *sccsid = "@(#)wwpty.c	3.3 83/08/26";
#endif

#include "ww.h"

wwgetpty(w)
	register struct ww *w;
{
	register char c;
	register char *line;
	register int i;
#define PTY "/dev/ptyXX"

	for (c = 'p'; c <= 's'; c++) {
		line = PTY;
		line[sizeof PTY - 6] = 'p';
		line[sizeof PTY - 3] = c;
		line[sizeof PTY - 2] = '0';
		if (access(line, 0) < 0)
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
				(void) close(w->ww_pty);
			}
		}
	}
	wwerrno = WWE_NOPTY;
	return -1;
good:
	(void) strcpy(w->ww_ttyname, line);
	return 0;
}
