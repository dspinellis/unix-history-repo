#ifndef lint
static	char *sccsid = "@(#)cmd3.c	3.8 84/01/13";
#endif

#include "defs.h"
#include "value.h"
#include "var.h"
#include "string.h"

c_variable()
{
	register struct ww *w;
	int printvar();

	if ((w = openiwin(wwnrow - 3, "Variables")) == 0) {
		error("Can't open variable window: %s.", wwerror());
		return;
	}
	if (var_walk(printvar, (int)w) >= 0)
		waitnl(w);
	closeiwin(w);
}

printvar(w, r)
register struct ww *w;
register struct var *r;
{
	if (more(w, 0) == 2)
		return -1;
	wwprintf(w, "%16s\t", r->r_name);
	switch (r->r_val.v_type) {
	case V_STR:
		wwprintf(w, "%s\n", r->r_val.v_str);
		break;
	case V_NUM:
		wwprintf(w, "%d\n", r->r_val.v_num);
		break;
	case V_ERR:
		wwprintf(w, "ERROR\n");
		break;
	}
	return 0;
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
