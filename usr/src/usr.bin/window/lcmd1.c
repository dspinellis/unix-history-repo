#ifndef lint
static	char *sccsid = "@(#)lcmd1.c	3.23 84/05/06";
#endif

#include "defs.h"
#include "string.h"
#include "value.h"
#include "lcmd.h"
#include "var.h"

struct lcmd_arg arg_window[] = {
	{ "row",	1,	ARG_ANY },
	{ "column",	1,	ARG_ANY },
	{ "nrows",	2,	ARG_ANY },
	{ "ncols",	2,	ARG_ANY },
	{ "nlines",	2,	ARG_NUM },
	{ "label",	1,	ARG_STR },
	{ "shell",	1,	ARG_STR },
	{ "pty",	1,	ARG_ANY },
	{ "frame",	1,	ARG_ANY },
	0
};

l_window(v, a)
register struct value *v, *a;
{
	int col, row, ncol, nrow, id, nline;
	char *label;
	char haspty, hasframe;
	char *shf, **sh;
	char *argv[sizeof shell / sizeof *shell];

	if ((id = findid()) < 0)
		return;
	row = a[0].v_type != V_NUM ? 1 : a[0].v_num;
	col = a[1].v_type != V_NUM ? 0 : a[1].v_num;
	nrow = a[2].v_type != V_NUM ? wwnrow - row : a[2].v_num;
	ncol = a[3].v_type != V_NUM ? wwncol - col : a[3].v_num;
	nline = a[4].v_type == V_ERR ? nbufline : a[4].v_num;
	label =  a[5].v_type == V_ERR ? 0 : a[5].v_str;
	if (a[6].v_type == V_STR) {
		if (mkargv(a[6].v_str, argv, sizeof argv / sizeof *argv) < 0)
			return;
		sh = argv;
		shf = *argv;
	} else {
		sh = shell;
		shf = shellfile;
	}
	if ((haspty = vtobool(a + 7, 1, -1)) < 0)
		return;
	if ((hasframe = vtobool(a + 8, 1, -1)) < 0)
		return;
	if (openwin(id, row, col, nrow, ncol, nline, label,
			haspty, hasframe, shf, sh) == 0)
		return;
	v->v_type = V_NUM;
	v->v_num = id + 1;
}

struct lcmd_arg arg_nline[] = {
	{ "nlines",	1,	ARG_NUM },
	0
};

l_nline(v, a)
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
	0
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
	char buf[2];

	buf[0] = escapec;
	buf[1] = 0;
	if ((v->v_str = str_cpy(buf)) == 0) {
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
struct value *v;
register struct value *a;
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
	addwin(w, flag);
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
	setterse(vtobool(a, 1, terse));
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

/*ARGSUSED*/
l_write(v, a)
struct value *v;
register struct value *a;
{
	char buf[20];
	struct ww *w;

	if ((w = vtowin(a++)) == 0)
		return;
	while (a->v_type != V_ERR) {
		if (a->v_type == V_NUM) {
			(void) sprintf(buf, "%d", a->v_num);
			(void) write(w->ww_pty, buf, strlen(buf));
		} else
			(void) write(w->ww_pty, a->v_str, strlen(a->v_str));
		if ((++a)->v_type != V_ERR)
			(void) write(w->ww_pty, " ", 1);
	}
}

/*ARGSUSED*/
l_close(v, a)
struct value *v;
register struct value *a;
{
	struct ww *w;

	if (a->v_type == V_STR && str_match(a->v_str, "all", 1))
		c_close((struct ww *)0);
	else
		for (; a->v_type != V_ERR; a++)
			if ((w = vtowin(a)) != 0)
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
		error("No window specified.");
		return 0;
	case V_STR:
		error("%s: No such window.", v->v_str);
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

mkargv(p, argv, n)
register char *p;
register char **argv;
register n;
{
	while (--n > 0) {
		for (; *p && (*p == ' ' || *p == '\t'); p++)
			;
		if (!*p)
			break;
		*argv++ = p;
		for (; *p && *p != ' ' && *p != '\t'; p++)
			;
		if (*p)
			*p++ = 0;
	}
	if (n == 0) {
		error("Too many shell arguments.");
		return -1;
	}
	*argv = 0;
	return 0;
}
