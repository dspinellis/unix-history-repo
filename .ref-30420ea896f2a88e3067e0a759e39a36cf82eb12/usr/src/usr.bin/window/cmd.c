#ifndef lint
static	char *sccsid = "@(#)cmd.c	3.13 83/09/15";
#endif

#include "defs.h"

docmd()
{
	register char c;
	register struct ww *w;

	if (!terse)
		wwadd(cmdwin, &wwhead);
	if (selwin != 0)
		wwcursor(selwin, 1);
	for (;;) {
		while ((c = bgetc()) >= 0) {
			if (!terse)
				(void) wwputs("\r\n", cmdwin);
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
		}
		if (!incmd || quit)
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
	if (!quit) {
		if (!terse) {
			wwdelete(cmdwin);
			reframe();
		}
		wwcursor(selwin, 0);
	}
}

struct ww *
getwin()
{
	register int c;
	struct ww *w = 0;

	if (!terse)
		(void) wwputs("Which window? ", cmdwin);
	wwcurtowin(cmdwin);
	while ((c = bgetc()) < 0)
		bread();
	if (debug && c == 'c')
		w = cmdwin;
	else if (debug && c == 'f')
		w = framewin;
	else if (debug && c == 'b')
		w = boxwin;
	else if (c >= '1' && c < NWINDOW + '1')
		w = window[c - '1'];
	if (w == 0)
		wwbell();
	if (!terse)
		(void) wwputs("\r\n", cmdwin);
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
	char moved = 0;

	if (wwvisible(w))
		moved = 1;
	else
		while (w->ww_back != framewin) {
			wwmoveup(w);
			moved = 1;
		}
	if (moved)
		reframe();
}

reframe()
{
	register struct ww *w;

	wwunframe(framewin);
	for (w = wwhead.ww_back; w != &wwhead; w = w->ww_back)
		if (w->ww_hasframe) {
			wwframe(w, framewin);
			labelwin(w);
		}
}

labelwin(w)
register struct ww *w;
{
	int mode = w == selwin ? WWM_REV : 0;

	if (w->ww_id >= 0) {
		char buf[2];

		buf[0] = w->ww_id + '1';
		buf[1] = 0;
		wwlabel(w, framewin, 1, buf, mode);
	}
	if (w->ww_label) {
		int col;

		if (w->ww_center) {
			col = (w->ww_w.nc - strlen(w->ww_label)) / 2;
			col = MAX(3, col);
		} else
			col = 3;
		wwlabel(w, framewin, col, w->ww_label, mode);
	}
}
