#ifndef lint
static	char *sccsid = "@(#)cmd.c	2.1.1.1 83/08/09";
#endif

#include "defs.h"

struct ww *getwin();

docmd()
{
	register char c;
	register struct ww *w;

	if (!terse)
		wwadd(cmdwin, &wwhead);
	/*
	if (selwin != 0)
		Woncursor(selwin->ww_win, 1);
	*/
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
		case CTRL(u):
		case CTRL(d):
		case CTRL(b):
		case CTRL(f):
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
		case 'Z':
			error("Command Z is now C.");
			break;
		case 'w':
			c_window();
			break;
		case 'S':
			c_show();
			break;
		case 'L':
			c_list();
			break;
		case 's':
			c_stat();
			break;
		case 'M':
			wwdumpsmap();
			break;
		case 'V':
			if ((w = getwin()) != 0)
				wwdumpnvis(w);
			break;
		case 'D':
			if ((w = getwin()) != 0)
				wwdumpcov(w);
			break;
		case 'W':
			if ((w = getwin()) != 0)
				wwdumpwin(w);
			break;
		case 't':
			c_time(RUSAGE_SELF);
			break;
		case 'T':
			c_time(RUSAGE_CHILDREN);
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
		/*
		case CTRL(d):
			c_scroll(1);
			break;
		case CTRL(u):
			c_scroll(-1);
			break;
		case CTRL(f):
			c_scroll(2);
			break;
		case CTRL(b):
			c_scroll(-2);
			break;
		*/
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
		case '.':
			error("Use q to quit.");
			break;
		default:
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
	/*
	if (selwin != 0)
		Woncursor(selwin->ww_win, 0);
	*/
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
	else if (c >= '1' && c < NWINDOW + '1')
		w = window[c - '1'];
	if (w == 0)
		wwbell();
	if (!terse)
		(void) wwputs("\r\n", cmdwin);
	return w;
}

setselwin(w)
register struct ww *w;
{
	register struct ww *oldselwin = selwin;

	if (w == oldselwin)
		return;
	if (selwin = w) {
		wwdelete(w);
		/*
		 * Stick it in front of the old selected window,
		 * or behind everbody else.
		 */
		wwadd(w, (oldselwin ? oldselwin : &wwhead)->ww_back);
		/*
		Woncursor(w->ww_win, 1);
		*/
	}
	/*
	if (oldselwin) {
		Woncursor(oldselwin->ww_win, 0);
	}
	*/
	reframe();
}

labelwin(w)
register struct ww *w;
{
	int mode = w == selwin ? WWM_REV : 0;

	if (w->ww_id >= 0) {
		char buf[2];

		buf[0] = w->ww_id + '1';
		buf[1] = 0;
		(void) wwlabel(w, framewin, 1, buf, mode);
	}
	if (w->ww_label) {
		int col;

		if (w->ww_center) {
			col = (w->ww_w.nc - strlen(w->ww_label)) / 2;
			col = MAX(3, col);
		} else
			col = 3;
		(void) wwlabel(w, framewin, col, w->ww_label, mode);
	}
}
