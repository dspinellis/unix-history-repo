#ifndef lint
static	char *sccsid = "@(#)cmd.c	1.9 83/07/28";
#endif

#include "defs.h"

struct ww *getwin();

docmd()
{
	register char c;
	register struct ww *w;

	if (!terse)
		Wunhide(cmdwin->ww_win);
	if (selwin != 0)
		Woncursor(selwin->ww_win, 1);
top:
	while ((c = bgetc()) >= 0) {
		if (!terse)
			wwputs("\r\n", cmdwin);
		switch (c) {
		default:
			if (c == escapec)
				goto foo;
			break;
		case 'h': case 'j': case 'k': case 'l':
		case CTRL(u):
		case CTRL(d):
		case CTRL(b):
		case CTRL(f):
		case CTRL([):
		foo:
			if (selwin == 0) {
				if (terse)
					Ding();
				else
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
			if ((w = getwin()) != 0)
				setselwin(w);
			break;
		case 'c':
			if ((w = getwin()) != 0)
				doclose(w);
			break;
		case 'C':
			doclose((struct ww *)0);
			break;
		case 'Z':
			if (terse)
				Ding();
			else
				wwputs("Command Z is now C.  ", cmdwin);
			break;
		case 'w':
			dowindow();
			break;
		case 'S':
			doshow();
			break;
		case 'L':
			dolist();
			break;
		/*
		case 'e':
			doescape();
			break;
		case 'L':
			dolabel();
			break;
		case 'r':
			selwin->ww_refresh = 0;
			break;
		case 'R':
			selwin->ww_refresh = 1;
			break;
		*/
		case 's':
			dostat();
			break;
		case 't':
			dotime(RUSAGE_SELF);
			break;
		case 'T':
			dotime(RUSAGE_CHILDREN);
			break;
		case ':':
			docolon();
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
		case CTRL([):
			goto out;
		case CTRL(z):
			wwsuspend();
			break;
		case 'q':
			doquit();
			if (quit)
				goto out;
			break;
		case '.':
			if (terse)
				Ding();
			else
				wwputs("Use q to quit.  ", cmdwin);
			break;
		default:
			if (c == escapec) {
				write(selwin->ww_pty, &escapec, 1);
				goto out;
			}
			Ding();
			if (!terse)
				wwprintf(cmdwin, "Type ? for help.  ");
			break;
		}
	}
	if (terse)
		wwsetcursor(0, 0);
	else {
		if (!terse)
			wwputs("Command: ", cmdwin);
		wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	}
	while (bpeekc() < 0)
		bread();
	goto top;
out:
	if (!quit)
		wwsetcurwin(selwin);
	if (selwin != 0)
		Woncursor(selwin->ww_win, 0);
	if (!terse)
		Whide(cmdwin->ww_win);
}

struct ww *
getwin()
{
	register int c;
	struct ww *w = 0;

	if (!terse)
		wwputs("Which window? ", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	while ((c = bgetc()) < 0)
		bread();
	if (c < '1' || c > '9' || (w = wwfind(c - '0')) == 0)
		Ding();
	if (!terse)
		wwputs("\r\n", cmdwin);
	return w;
}

setselwin(w)
register struct ww *w;
{
	struct ww *oldselwin = selwin;

	if (w == oldselwin)
		return;
	if (selwin = w) {
		labelwin(w);
		/* bring it to the top just below cmdwin */
		wwsetcurwin(w);
		wwsetcurwin(cmdwin);
		Woncursor(w->ww_win, 1);
	}
	if (oldselwin) {
		labelwin(oldselwin);
		Woncursor(oldselwin->ww_win, 0);
	}
}

labelwin(w)
register struct ww *w;
{
	char buf[2];
	int mode = w == selwin ? WINVERSE : 0;

	buf[0] = w->ww_ident + '0';
	buf[1] = 0;
	wwlabel(w, 1, buf, mode);
	if (w->ww_label)
		wwlabel(w, 3, w->ww_label, mode);
}
