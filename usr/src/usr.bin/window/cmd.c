#ifndef lint
static	char *sccsid = "@(#)cmd.c	3.9 83/08/31";
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
top:
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
			goto out;
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
			goto out;
		case CTRL(z):
			wwsuspend();
			break;
		case 'q':
			c_quit();
			if (quit)
				goto out;
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
				(void) write(selwin->ww_pty, &escapec, 1);
				goto out;
			}
			if (!terse)
				wwbell();
			error("Type ? for help.");
			break;
		}
	}
	if (terse)
		wwsetcursor(0, 0);
	else {
		if (!terse)
			(void) wwputs("Command: ", cmdwin);
		wwsetcursor(wwcurrow(cmdwin), wwcurcol(cmdwin));
	}
	while (bpeekc() < 0)
		bread();
	goto top;
out:
	if (!quit) {
		curwin = selwin;
		if (!terse) {
			wwdelete(cmdwin);
			reframe();
		}
	}
	if (selwin != 0)
		wwcursor(selwin, 0);
}

struct ww *
getwin()
{
	register int c;
	struct ww *w = 0;

	if (!terse)
		(void) wwputs("Which window? ", cmdwin);
	wwsetcursor(wwcurrow(cmdwin), wwcurcol(cmdwin));
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

setselwin(w)
struct ww *w;
{
	if ((selwin = w) != 0)
		front(w);
}

front(w)
register struct ww *w;
{
	char moved = 0;

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
