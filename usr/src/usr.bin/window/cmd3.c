#ifndef lint
static	char *sccsid = "@(#)cmd3.c	3.6 83/12/06";
#endif

#include "defs.h"
#include "value.h"
#include "var.h"
#include "string.h"

#define VLINE (wwnrow - 3)
static vlineno;
static struct ww *vw;

c_variable()
{
	int printvar();

	if ((vw = openiwin(VLINE, "Variables")) == 0) {
		error("Can't open variable window: %s.", wwerror());
		return;
	}
	vlineno = 0;
	var_walk(printvar);
	waitnl(vw);
	closeiwin(vw);
}

printvar(r)
register struct var *r;
{
	if (vlineno >= VLINE - 2)
		waitnl(vw);
	wwprintf(vw, "%16s\t", r->r_name);
	switch (r->r_val.v_type) {
	case V_STR:
		wwprintf(vw, "%s\n", r->r_val.v_str);
		break;
	case V_NUM:
		wwprintf(vw, "%d\n", r->r_val.v_num);
		break;
	case V_ERR:
		wwprintf(vw, "ERR\n");
		break;
	}
}

struct ww *getwin();
struct ww *openwin();
char *strtime();

doclose(w)
register struct ww *w;
{
	char didit = 0;
	struct ww *w1;

	if (w != 0) {
		if (w == selwin)
			setselwin(0);
		wwclose(w);
		didit++;
	} else {
		for (w = wwhead; w;) {
			if (w == cmdwin) {
				w = w->ww_next;
				continue;
			}
			w = (w1 = w)->ww_next;
			if (w1 == selwin)
				setselwin(0);
			if (w->ww_state == WW_HASPROC && w->ww_pid == 0) {
				wwprintf(cmdwin, "%d: pid == 0.  ",
					w->ww_ident);
			} else {
				wwclose(w1);
				didit++;
			}
		}
	}
	if (selwin == 0) {
		for (w = wwhead; w && w == cmdwin; w = w->ww_next)
	}
	if (didit)
		reframe();
}

/*
doescape()
{
	char buf[2];

	wwputs("New escape character? ", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	while ((*buf = bgetc()) < 0)
		bread();
	buf[1] = 0;
	setescape(buf);
	wwputs("\r\n", cmdwin);
}
*/

setescape(esc)
register char *esc;
{
	if (*esc == '^') {
		if (esc[1] != 0)
			escapec = esc[1] & 0x1f;
		else
			escapec = '^';
	} else
		escapec = *esc;
}

/*
dolabel()
{
	register struct ww *w;
	char buf[30];
	char *malloc();

	if ((w = getwin()) == 0)
		return;
	wwprintf(cmdwin, "Label for window %d? ", w->ww_ident);
	bgets(buf, sizeof buf, cmdwin);
	setlabel(w, buf);
	wwputs("\r\n", cmdwin);
}
*/

setlabel(w, label)
register struct ww *w;
char *label;
{
	if (w->ww_label != 0)
}
