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

c_close(w)
register struct ww *w;
{
	char didit = 0;
	register i;

	if (w != 0) {
		closewin(w);
		didit++;
	} else {
		for (i = 0; i < NWINDOW; i++) {
			if ((w = window[i]) == 0)
				continue;
			closewin(w);
			didit++;
		}
	}
	if (selwin == 0) {
		if (lastselwin != 0)
			setselwin(lastselwin);
		else {
			for (i = 0; i < NWINDOW && window[i] == 0; i++)
				;
			if (i < NWINDOW)
				setselwin(window[i]);
		}
	}
	if (didit)
		reframe();
}

closewin(w)
register struct ww *w;
{
	if (w == selwin)
		selwin = 0;
	if (w == lastselwin)
		lastselwin = 0;
	if (w->ww_id >= 0 && w->ww_id < NWINDOW)
		window[w->ww_id] = 0;
	if (w->ww_label)
		free(w->ww_label);
	wwdelete(w);
	wwclose(w);
}

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

setlabel(w, label)
register struct ww *w;
char *label;
{
	if (w->ww_label != 0)
		str_free(w->ww_label);
	if ((w->ww_label = str_cpy(label)) == 0)
		return -1;
	return 0;
}
