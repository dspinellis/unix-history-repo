#ifndef lint
static	char *sccsid = "@(#)lcmd1.c	3.16 84/03/29";
#endif

#include "defs.h"
#include "string.h"
#include "value.h"
#include "lcmd.h"

struct lcmd_arg arg_window[] = {
	{ "row",	1,	ARG_ANY },
	{ "column",	1,	ARG_ANY },
	{ "nrows",	2,	ARG_ANY },
	{ "ncols",	2,	ARG_ANY },
	{ "nlines",	2,	ARG_NUM },
	{ "label",	1,	ARG_STR },
	0
};

l_window(v, a)
register struct value *v, *a;
{
	int col, row, ncol, nrow, id, nline;
	char *label;

	if ((id = findid()) < 0)
		return;
	row = a->v_type != V_NUM ? 1 : a->v_num;
	col = (++a)->v_type != V_NUM ? 0 : a->v_num;
	nrow = (++a)->v_type != V_NUM ? wwnrow - row : a->v_num;
	ncol = (++a)->v_type != V_NUM ? wwncol - col : a->v_num;
	nline = (++a)->v_type == V_ERR ? nbufline : a->v_num;
	label =  (++a)->v_type == V_ERR ? 0 : a->v_str;
	if (openwin(id, row, col, nrow, ncol, nline, label) == 0)
		return;
	v->v_type = V_NUM;
	v->v_num = id + 1;
}

struct lcmd_arg arg_buffer[] = {
	{ "nlines",	1,	ARG_NUM },
	0
};

l_buffer(v, a)
register struct value *v, *a;
{
	v->v_num = nbufline;
	v->v_type = V_NUM;
	if (a->v_type != V_ERR)
		nbufline = a->v_num;
}

struct lcmd_arg arg_select[] = {
	{ "window",	1,	ARG_NUM },
	0
};

l_select(v, a)
register struct value *v, *a;
{
	struct ww *w;

	v->v_type = V_NUM;
	v->v_num = selwin ? selwin->ww_id + 1 : -1;
	if (a->v_type == V_ERR)
		return;
	if ((w = vtowin(a)) == 0)
		return;
	setselwin(w);
}

struct lcmd_arg arg_debug[] = {
	{ "flag",	1,	ARG_ANY },
	{ 0,		0,	0 }
};

l_debug(v, a)
register struct value *v, *a;
{
	v->v_type = V_NUM;
	v->v_num = debug;
	debug = vtobool(a, 1, debug);
}

struct lcmd_arg arg_escape[] = {
	{ "escapec",	1,	ARG_STR },
	0
};

l_escape(v, a)
register struct value *v, *a;
{
	if ((v->v_str = str_cpy(unctrl(escapec))) == 0) {
		error("Out of memory.");
		return;
	}
	v->v_type = V_STR;
	if (a->v_type != V_ERR)
		setescape(a->v_str);
}

struct lcmd_arg arg_label[] = {
	{ "window",	1,	ARG_NUM },
	{ "label",	1,	ARG_STR },
	0
};

/*ARGSUSED*/
l_label(v, a)
register struct value *v, *a;
{
	struct ww *w;

	if ((w = vtowin(a)) == 0)
		return;
	if ((++a)->v_type != V_ERR && setlabel(w, a->v_str) < 0)
		error("Out of memory.");
	reframe();
}

struct lcmd_arg arg_foreground[] = {
	{ "window",	1,	ARG_NUM },
	{ "flag",	1,	ARG_ANY },
	0
};

l_foreground(v, a)
register struct value *v, *a;
{
	struct ww *w;
	char flag;

	if ((w = vtowin(a)) == 0)
		return;
	v->v_type = V_NUM;
	v->v_num = isfg(w);
	flag = vtobool(++a, 1, v->v_num);
	if (flag == v->v_num)
		return;
	deletewin(w);
	addwin(w, flag ? 0 : 1);
	reframe();
}

struct lcmd_arg arg_background[] = {
	{ "window",	1,	ARG_NUM },
	{ "flag",	1,	ARG_ANY },
	0
};

l_background(v, a)
register struct value *v, *a;
{
	struct ww *w;
	char flag;

	if ((w = vtowin(a)) == 0)
		return;
	v->v_type = V_NUM;
	v->v_num = isbg(w);
	flag = vtobool(++a, 1, v->v_num);
	if (flag == v->v_num)
		return;
	deletewin(w);
	addwin(w, flag ? 3 : 2);
	reframe();
}

struct lcmd_arg arg_terse[] = {
	{ "flag",	1,	ARG_ANY },
	0
};

l_terse(v, a)
register struct value *v, *a;
{
	v->v_type = V_NUM;
	v->v_num = terse;
	terse = vtobool(a, 1, terse);
	if (!terse && v->v_num)
		wwadd(cmdwin, &wwhead);
	else if (!v->v_num && terse)
		wwdelete(cmdwin);
	reframe();
}

struct lcmd_arg arg_source[] = {
	{ "filename",	1,	ARG_STR },
	0
};

l_source(v, a)
register struct value *v, *a;
{
	v->v_type = V_NUM;
	if (a->v_type != V_ERR && dosource(a->v_str) < 0) {
		error("Can't open %s.", a->v_str);
		v->v_num = -1;
	} else
		v->v_num = 0;
}

struct lcmd_arg arg_write[] = {
	{ "window",	1,	ARG_NUM },
	{ "string",	1,	ARG_STR },
	0
};

/*ARGSUSED*/
l_write(v, a)
register struct value *v, *a;
{
	struct ww *w;

	if ((w = vtowin(a)) == 0)
		return;
	a++;
	(void) write(w->ww_pty, a->v_str, strlen(a->v_str));
}

struct lcmd_arg arg_close[] = {
	{ "window",	1,	ARG_NUM },
	0
};

/*ARGSUSED*/
l_close(v, a)
register struct value *v, *a;
{
	struct ww *w;

	if (a->v_type == V_ERR)
		c_close((struct ww *)0);
	else if ((w = vtowin(a)) != 0)
		c_close(w);
}

struct lcmd_arg arg_cursormodes[] = {
	{ "modes",	1,	ARG_NUM },
	0
};

l_cursormodes(v, a)
register struct value *v, *a;
{

	v->v_type = V_NUM;
	v->v_num = wwcursormodes;
	if (a->v_type != V_ERR)
		wwsetcursormodes(a->v_num);
}

struct lcmd_arg arg_unset[] = {
	{ "variable",	1,	ARG_ANY },
	0
};

l_unset(v, a)
register struct value *v, *a;
{
	v->v_type = V_NUM;
	switch (a->v_type) {
	case V_ERR:
		v->v_num = -1;
		return;
	case V_NUM:
		if ((a->v_str = str_itoa(a->v_num)) == 0) {
			error("Out of memory.");
			v->v_num = -1;
			return;
		}
		a->v_type = V_STR;
		break;
	}
	v->v_num = var_unset(a->v_str);
}

struct ww *
vtowin(v)
register struct value *v;
{
	struct ww *w;

	switch (v->v_type) {
	case V_ERR:
		error("Window identifier required.");
		return 0;
	case V_STR:
		error("Number required for window identifier.");
		return 0;
	}
	if (v->v_num < 1 || v->v_num > NWINDOW
	    || (w = window[v->v_num - 1]) == 0) {
		error("%d: No such window.", v->v_num);
		return 0;
	}
	return w;
}

vtobool(v, def, err)
register struct value *v;
char def, err;
{
	switch (v->v_type) {
	case V_NUM:
		return v->v_num != 0;
	case V_STR:
		if (str_match(v->v_str, "true", 1)
		    || str_match(v->v_str, "on", 2)
		    || str_match(v->v_str, "yes", 1))
			return 1;
		else if (str_match(v->v_str, "false", 1)
		    || str_match(v->v_str, "off", 2)
		    || str_match(v->v_str, "no", 1))
			return 0;
		else {
			error("%s: Illegal boolean value.", v->v_str);
			return err;
		}
		/*NOTREACHED*/
	case V_ERR:
		return def;
	}
	/*NOTREACHED*/
}
