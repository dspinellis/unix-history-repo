#ifndef lint
static	char *sccsid = "@(#)cmd3.c	3.9 84/01/13";
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
	(void) wwprintf(w, "%16s\t", r->r_name);
	switch (r->r_val.v_type) {
	case V_STR:
		(void) wwprintf(w, "%s\n", r->r_val.v_str);
		break;
	case V_NUM:
		(void) wwprintf(w, "%d\n", r->r_val.v_num);
		break;
	case V_ERR:
		(void) wwprintf(w, "ERROR\n");
		break;
	}
	return 0;
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
