#ifndef lint
static	char *sccsid = "@(#)cmd.c	1.4 83/07/22";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

docmd()
{
	register char c;
	register struct ww *w;
	char buf;

top:
	Wunhide(cmdwin->ww_win);
	if (selwin != 0)
		Woncursor(selwin->ww_win, 1);
	while ((c = bgetc()) >= 0) {
		wwputs("\r\n", cmdwin);
		switch (c) {
		case 'r':
		case 'R':
		case 'h': case 'j': case 'k': case 'l':
		case CTRL(u):
		case CTRL(d):
		case CTRL(b):
		case CTRL(f):
		case CTRL([):
		case ESCAPE:
			if (selwin == 0) {
				wwputs("No window.  ", cmdwin);
				continue;
			}
		}
		switch (c) {
		case '1': case '2': case '3': case '4': case '5':
		case '6': case '7': case '8': case '9':
			if ((w = wwfind(c - '0')) == 0) {
				Ding();
				break;
			}
			setselwin(w);
			goto out;
		case '%':
			if ((w = getwin()) == 0)
				break;
			setselwin(w);
			break;
		case 'c':
			doclose(CLOSE_ONE, getwin());
			break;
		case 'C':
			doclose(CLOSE_DEAD, (struct ww *)0);
			break;
		case 'Z':
			doclose(CLOSE_ALL, (struct ww *)0);
			break;
		case 'w':
			dowindow();
			break;
		case 'Q':
			doquery();
			break;
		case 'r':
			selwin->ww_refresh = 0;
			break;
		case 'R':
			selwin->ww_refresh = 1;
			break;
		case 't':
			dotime(RUSAGE_SELF);
			break;
		case 'T':
			dotime(RUSAGE_CHILDREN);
			break;
		case 'h':
			Wcurleft(selwin->ww_win, 1);
			break;
		case 'j':
			Wcurdown(selwin->ww_win, 1);
			break;
		case 'k':
			Wcurup(selwin->ww_win, 1);
			break;
		case 'l':
			Wcurright(selwin->ww_win, 1);
			break;
		case CTRL(d):
			doscroll(1);
			break;
		case CTRL(u):
			doscroll(-1);
			break;
		case CTRL(f):
			doscroll(2);
			break;
		case CTRL(b):
			doscroll(-2);
			break;
		case CTRL(l):
			ScreenGarbaged = 1;
			break;
		case '?':
			dohelp();
			break;
		case ESCAPE:
			buf = ESCAPE;
			write(selwin->ww_pty, &buf, 1);
		case CTRL([):
			goto out;
		case CTRL(z):
			wwsuspend();
			break;
		case '.':
			doquit();
			if (quit)
				goto out;
			break;
		default:
			Ding();
			wwprintf(cmdwin, "Type ? for help.  ");
			break;
		}
	}
	wwputs("Command: ", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	while (bpeekc() < 0)
		bread();
	goto top;
out:
	if (!quit)
		wwsetcurwin(selwin);
	if (selwin != 0)
		Woncursor(selwin->ww_win, 0);
	Whide(cmdwin->ww_win);
}

struct ww *
getwin()
{
	register int c;
	struct ww *w;

	while ((c = bgetc()) < 0)
		bread();
	if (c < '1' || c > '9') {
		Ding();
		return 0;
	}
	if ((w = wwfind(c - '0')) == 0)
		Ding();
	return w;
}

setselwin(w)
register struct ww *w;
{
	if (selwin) {
		labelwin(selwin, 0);
		Woncursor(selwin->ww_win, 0);
	}
	if (selwin = w) {
		labelwin(w, WINVERSE);
		/* bring it to the top just below cmdwin */
		wwsetcurwin(w);
		wwsetcurwin(cmdwin);
		Woncursor(w->ww_win, 1);
	}
}

labelwin(w, mode)
register struct ww *w;
{
	char buf[2];

	buf[0] = w->ww_ident + '0';
	buf[1] = 0;
	wwlabel(w, buf, mode);
}
