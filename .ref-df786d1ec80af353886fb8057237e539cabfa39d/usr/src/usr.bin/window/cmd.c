#ifndef lint
static	char *sccsid = "@(#)cmd.c	3.13 83/09/15";
#endif

#include "defs.h"

docmd()
{
	register char c;
	register struct ww *w;

	if (!terse)
		Wunhide(cmdwin->ww_win);
	if (selwin != 0)
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
					error("No window.");
					continue;
				}
			}
			switch (c) {
			case '1': case '2': case '3': case '4': case '5':
			case '6': case '7': case '8': case '9':
				if ((w = window[c - '1']) == 0) {
					wwbell();
					break;
				}
				setselwin(w);
				if (checkproc(selwin) >= 0)
					incmd = 0;
				break;
			case '%':
				if ((w = getwin()) != 0)
					setselwin(w);
				break;
			case 'c':
				if ((w = getwin()) != 0)
					c_close(w);
				break;
			case 'C':
				c_close((struct ww *)0);
				break;
			case 'w':
				c_window();
				break;
			case 'm':
				if ((w = getwin()) != 0)
					c_move(w);
				break;
			case 'M':
				if ((w = getwin()) != 0)
					movewin(w, w->ww_altpos.r,
						w->ww_altpos.c);
				break;
			case 'S':
				c_show();
				break;
			case 'L':
				c_list();
				break;
			case ':':
				c_colon();
				break;
			case 'h':
				(void) wwwrite(selwin, "\b", 1);
				break;
			case 'j':
				(void) wwwrite(selwin, "\n", 1);
				break;
			case 'k':
				(void) wwwrite(selwin, "\033A", 2);
				break;
			case 'l':
				(void) wwwrite(selwin, "\033C", 2);
				break;
			case CTRL(e):
				wwscroll(selwin, 1);
				break;
			case CTRL(y):
				wwscroll(selwin, -1);
				break;
			case CTRL(d):
				wwscroll(selwin, selwin->ww_w.nr / 2);
				break;
			case CTRL(u):
				wwscroll(selwin, - selwin->ww_w.nr / 2);
				break;
			case CTRL(f):
				wwscroll(selwin, selwin->ww_w.nr);
				break;
			case CTRL(b):
				wwscroll(selwin, - selwin->ww_w.nr);
				break;
			case CTRL(s):
				(void) write(selwin->ww_pty,
					&wwwintty.ww_tchars.t_stopc, 1);
				break;
			case CTRL(q):
				(void) write(selwin->ww_pty,
					&wwwintty.ww_tchars.t_startc, 1);
				break;
			case CTRL(l):
				wwredraw();
				break;
			case '?':
				c_help();
				break;
			case CTRL([):
				if (checkproc(selwin) >= 0)
					incmd = 0;
				break;
			case CTRL(z):
				wwsuspend();
				break;
			case 'q':
				c_quit();
				break;
			/* undocumented commands */
			case 's':
				c_stat();
				break;
			case 't':
				c_time(RUSAGE_SELF);
				break;
			case 'T':
				c_time(RUSAGE_CHILDREN);
				break;
			/* debugging stuff */
			case '&':
				if (debug) {
					c_debug();
					break;
				}
			default:
				if (c == escapec) {
					if (checkproc(selwin) >= 0) {
						(void) write(selwin->ww_pty,
							&escapec, 1);
						incmd = 0;
					}
				} else {
					if (!terse)
						wwbell();
					error("Type ? for help.");
				}
			}
			Ding();
			break;
		if (terse)
			wwsetcursor(0, 0);
		else {
			(void) wwputs("Command: ", cmdwin);
			wwcurtowin(cmdwin);
		}
		while (bpeekc() < 0)
			bread();
	}
	if (!quit)
		wwsetcurwin(selwin);
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

checkproc(w)
struct ww *w;
{
	if (w->ww_state != WWS_HASPROC) {
		error("No process in window.");
		return -1;
	}
	return 0;
}

setselwin(w)
struct ww *w;
{
	if ((selwin = w) != 0)
		front(w);
}

/*
 * This is all heuristic.
 * wwvisible() doesn't work for tinted windows.
 * and wwmoveup() doesn't work for transparent windows
 * (completely or partially).
 * But anything to make it faster.
 */
front(w)
register struct ww *w;
{
	struct ww *oldselwin = selwin;

	else
		while (w->ww_back != framewin) {
			wwmoveup(w);
			moved = 1;
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
