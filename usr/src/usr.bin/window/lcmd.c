#ifndef lint
static	char *sccsid = "@(#)lcmd.c	3.18 84/04/08";
#endif

#include "defs.h"
#include "value.h"
#include "lcmd.h"

int l_close();
int l_cursormodes();
int l_debug();
int l_escape();
int l_foreground();
int l_iostat();
int l_label();
int l_list();
int l_nline();
int l_select();
int l_shell();
int l_source();
int l_terse();
int l_time();
int l_unset();
int l_variable();
int l_window();
int l_write();

struct lcmd_arg arg_cursormodes[];
struct lcmd_arg arg_debug[];
struct lcmd_arg arg_escape[];
struct lcmd_arg arg_foreground[];
struct lcmd_arg arg_label[];
struct lcmd_arg arg_nline[];
struct lcmd_arg arg_select[];
struct lcmd_arg arg_shell[];
struct lcmd_arg arg_source[];
struct lcmd_arg arg_terse[];
struct lcmd_arg arg_time[];
struct lcmd_arg arg_unset[];
struct lcmd_arg arg_window[];
struct lcmd_arg arg_null[] = 0;

struct lcmd_tab lcmd_tab[] = {
	"%",		1,	l_select,	arg_select,
	"buffer",	2,	l_nline,	arg_nline,
	"close",	2,	l_close,	0,
	"cursormodes",	2,	l_cursormodes,	arg_cursormodes,
	"debug",	1,	l_debug,	arg_debug,
	"escape",	1,	l_escape,	arg_escape,
	"foreground",	1,	l_foreground,	arg_foreground,
	"iostat",	1,	l_iostat,	arg_null,
	"label",	2,	l_label,	arg_label,
	"list",		2,	l_list,		arg_null,
	"nlines",	1,	l_nline,	arg_nline,
	"select",	2,	l_select,	arg_select,
	"shell",	2,	l_shell,	arg_shell,
	"source",	2,	l_source,	arg_source,
	"terse",	2,	l_terse,	arg_terse,
	"time",		2,	l_time,		arg_time,
	"unset",	1,	l_unset,	arg_unset,
	"variable",	1,	l_variable,	arg_null,
	"window",	2,	l_window,	arg_window,
	"write",	2,	l_write,	0,
	0
};

struct lcmd_tab *
lcmd_lookup(name)
char *name;
{
	register struct lcmd_tab *p;

	for (p = lcmd_tab; p->lc_name != 0; p++)
		if (str_match(name, p->lc_name, p->lc_minlen))
			return p;
	return 0;
}

dosource(filename)
char *filename;
{
	if (cx_beginfile(filename) < 0)
		return -1;
	p_start();
	err_end();
	cx_end();
	return 0;
}

dolongcmd(buffer)
char *buffer;
{
	if (cx_beginbuf(buffer) < 0)
		return -1;
	p_start();
	err_end();
	cx_end();
	return 0;
}
