#ifndef lint
static	char *sccsid = "@(#)lcmd1.c	3.12 84/01/11";
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
	{ 0,		0,	0 }
};

l_window(v)
register struct value *v;
{
	register struct lcmd_arg *a = arg_window;
	int col, row, ncol, nrow, id, nline;
	char *label;

	if ((id = findid()) < 0)
		return;
	row = a->arg_vtype != V_NUM ? 1 : a->arg_num;
	col = (++a)->arg_vtype != V_NUM ? 0 : a->arg_num;
	nrow = (++a)->arg_vtype != V_NUM ? wwnrow - row : a->arg_num;
	ncol = (++a)->arg_vtype != V_NUM ? wwncol - col : a->arg_num;
	nline = (++a)->arg_vtype == V_ERR ? nbufline : a->arg_num;
	label =  (++a)->arg_vtype == V_ERR ? 0 : a->arg_str;
	if (openwin(id, row, col, nrow, ncol, nline, label) == 0)
		return;
	v->v_type = V_NUM;
	v->v_num = id + 1;
}

struct lcmd_arg arg_buffer[] = {
	{ "nlines",	1,	ARG_NUM },
	{ 0,		0,	0 }
};

l_buffer(v)
struct value *v;
{
	v->v_num = nbufline;
	v->v_type = V_NUM;
	if (arg_buffer[0].arg_vtype != V_ERR)
		nbufline = arg_buffer[0].arg_num;
}

struct lcmd_arg arg_select[] = {
	{ "window",	1,	ARG_NUM },
	{ 0,		0,	0 }
};

l_select(v)
struct value *v;
{
	struct ww *w;

	v->v_type = V_NUM;
	v->v_num = selwin ? selwin->ww_id + 1 : -1;
	if (arg_select[0].arg_vtype == V_ERR)
		return;
	if ((w = vtowin(&arg_select[0].arg_val)) == 0)
		return;
	setselwin(w);
}

struct lcmd_arg arg_debug[] = {
	{ "flag",	1,	ARG_ANY },
	{ 0,		0,	0 }
};

l_debug(v)
struct value *v;
{
	v->v_type = V_NUM;
	v->v_num = debug;
	debug = vtobool(&arg_debug[0].arg_val, 1, debug);
}

struct lcmd_arg arg_escape[] = {
	{ "escapec",	1,	ARG_NUM },
	{ 0,		0,	0 }
};

l_escape(v)
struct value *v;
{
	if ((v->v_str = str_cpy(unctrl(escapec))) == 0) {
		error("Out of memory.");
		return;
	}
	v->v_type = V_STR;
	if (arg_escape[0].arg_type != V_ERR)
		setescape(arg_escape[0].arg_str);
}

struct lcmd_arg arg_label[] = {
	{ "window",	1,	ARG_NUM },
	{ "label",	1,	ARG_STR },
	{ 0,		0,	0 }
};

/*ARGSUSED*/
l_label(v)
struct value *v;
{
	struct ww *w;
	register struct lcmd_arg *a = arg_label;

	if ((w = vtowin(&a->arg_val)) == 0)
		return;
	if ((++a)->arg_vtype != V_ERR && setlabel(w, a->arg_str) < 0)
		error("Out of memory.");
	reframe();
}

struct lcmd_arg arg_terse[] = {
	{ "flag",	1,	ARG_ANY },
	{ 0,		0,	0 }
};

l_terse(v)
struct value *v;
{
	v->v_type = V_NUM;
	v->v_num = terse;
	terse = vtobool(&arg_terse[0].arg_val, 1, terse);
	if (!terse && v->v_num)
		wwadd(cmdwin, &wwhead);
	else if (!v->v_num && terse)
		wwdelete(cmdwin);
	reframe();
}

struct lcmd_arg arg_source[] = {
	{ "filename",	1,	ARG_STR },
	{ 0,		0,	0 }
};

/*ARGSUSED*/
l_source(v)
struct value *v;
{
	if (arg_source[0].arg_vtype != V_ERR
	    && dosource(arg_source[0].arg_str) < 0) {
		error("Can't open %s.", arg_source[0].arg_str);
		v->v_num = -1;
	} else
		v->v_num = 0;
	v->v_type = V_NUM;
}

struct lcmd_arg arg_write[] = {
	{ "window",	1,	ARG_NUM },
	{ "string",	1,	ARG_STR },
	{ 0,		0,	0 }
};

/*ARGSUSED*/
l_write(v)
struct value *v;
{
	register struct lcmd_arg *a = arg_write;
	struct ww *w;

	if ((w = vtowin(&a->arg_val)) == 0)
		return;
	a++;
	(void) write(w->ww_pty, a->arg_str, strlen(a->arg_str));
}

struct lcmd_arg arg_close[] = {
	{ "window",	1,	ARG_NUM },
	{ 0,		0,	0 }
};

/*ARGSUSED*/
l_close(v)
struct value *v;
{
	register struct lcmd_arg *a = arg_close;
	struct ww *w;

	if (a->arg_vtype == V_ERR)
		c_close((struct ww *)0);
	else if ((w = vtowin(&a->arg_val)) != 0)
		c_close(w);
}

struct lcmd_arg arg_cursormodes[] = {
	{ "modes",	1,	ARG_NUM },
	{ 0,		0,	0 }
};

l_cursormodes(v)
register struct value *v;
{
	register struct lcmd_arg *a = arg_cursormodes;

	v->v_type = V_NUM;
	v->v_num = wwcursormodes;
	if (a->arg_vtype != V_ERR)
		wwsetcursormodes(a->arg_num);
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
