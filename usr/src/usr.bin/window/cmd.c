#ifndef lint
static	char *sccsid = "@(#)cmd.c	3.24 84/04/05";
#endif

#include "defs.h"

docmd()
{
	register char c;
	register struct ww *w;
	char out = 0;

	for (;;) {
		while ((c = wwgetc()) >= 0) {
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
					error("No window.");
					continue;
				}
			}
			switch (c) {
			case '1': case '2': case '3': case '4': case '5':
			case '6': case '7': case '8': case '9':
				if ((w = window[c - '1']) == 0) {
					error("%c: No such window.", c);
					break;
				}
				setselwin(w);
				if (checkproc(selwin) >= 0)
					 out = 1;
				break;
			case '%':
				if ((w = getwin()) != 0)
					setselwin(w);
				break;
			case CTRL(^):
				if (lastselwin != 0) {
					setselwin(lastselwin);
					if (checkproc(selwin) >= 0)
						out = 1;
				} else
					error("No previous window.");
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
			case 'L':
				c_list();
				break;
			case 'v':
				c_variable();
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
					out = 1;
				break;
			case CTRL(z):
				wwsuspend();
				break;
			case 'q':
				c_quit();
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
						out = 1;
					}
				} else {
					if (!terse)
						wwputc(CTRL(g), cmdwin);
					error("Type ? for help.");
				}
			}
		}
		if (out || quit)
			break;
		if (terse)
			wwsetcursor(0, 0);
		else {
			wwputs("Command: ", cmdwin);
			wwcurtowin(cmdwin);
		}
		while (wwpeekc() < 0)
			wwiomux();
	}
	if (!quit)
		setcmd(0);
}

struct ww *
getwin()
{
	register int c;
	struct ww *w = 0;

	if (!terse)
		wwputs("Which window? ", cmdwin);
	wwcurtowin(cmdwin);
	while ((c = wwgetc()) < 0)
		wwiomux();
	if (debug && c == 'c')
		w = cmdwin;
	else if (debug && c == 'f')
		w = framewin;
	else if (debug && c == 'b')
		w = boxwin;
	else if (c >= '1' && c < NWINDOW + '1')
		w = window[c - '1'];
	if (w == 0)
		wwputc(CTRL(g), cmdwin);
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

setcmd(new)
char new;
{
	if (new && !incmd) {
		if (!terse)
			wwadd(cmdwin, &wwhead);
		if (selwin != 0)
			wwcursor(selwin, 1);
		wwcurwin = 0;
	} else if (!new && incmd) {
		if (!terse) {
			wwdelete(cmdwin);
			reframe();
		}
		if (selwin != 0)
			wwcursor(selwin, 0);
		wwcurwin = selwin;
	}
	incmd = new;
}

setterse(new)
char new;
{
	if (incmd)
		if (new && !terse) {
			wwdelete(cmdwin);
			reframe();
		} else if (!new && terse)
			wwadd(cmdwin, &wwhead);
	terse = new;
}

/*
 * Set the current window.
 */
setselwin(w)
struct ww *w;
{
	if (selwin == w)
		return;
	lastselwin = selwin;
	front(selwin = w, 1);
}
