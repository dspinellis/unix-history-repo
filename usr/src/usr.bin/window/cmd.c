#ifndef lint
static	char *sccsid = "@(#)cmd.c	3.7 83/08/26";
#endif

#include "defs.h"

docmd()
{
	register char c;
	register struct ww *w;

	if (!terse)
		Wunhide(cmdwin->ww_win);
	if (selwin != 0)
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
		case CTRL(y):
		case CTRL(e):
		case CTRL(u):
		case CTRL(d):
		case CTRL(b):
		case CTRL(f):
		case CTRL(s):
		case CTRL(q):
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
			break;
		case 't':
			c_time(RUSAGE_SELF);
			break;
		case 'T':
			c_time(RUSAGE_CHILDREN);
			break;
		/* debugging commands */
		case 'M':
			if (!debug)
				goto badcmd;
			wwdumpsmap();
			break;
		case 'V':
			if (!debug)
				goto badcmd;
			if ((w = getwin()) != 0)
				wwdumpnvis(w);
			break;
		case 'D':
			if (!debug)
				goto badcmd;
			if ((w = getwin()) != 0)
				wwdumpcov(w);
			break;
		case 'W':
			if (!debug)
				goto badcmd;
			if ((w = getwin()) != 0)
				wwdumpwin(w);
			break;
		default:
		badcmd:
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
struct ww *w;
{
	if ((selwin = w) != 0)
		front(w);
}

front(w)
register struct ww *w;
{
	struct ww *oldselwin = selwin;

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
