/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"

/*
 * Initialization of option values.
 * The option #defines in ex_vars.h are made
 * from this file by the script makeoptions.
 */
char	direct[32] =
	{ '/', 't', 'm', 'p' };
char	sections[32] =
	{ 'N', 'H', 'S', 'H' };
char	paragraphs[32] =
	{ 'I', 'P', 'L', 'P', 'P', 'P', 'Q', 'P', 'b', 'p' };
char	shell[32] =
	{ '/', 'b', 'i', 'n', '/', 's', 'h' };
char	ttytype[16] =
	{ 'd', 'u', 'm', 'b' };

short	COLUMNS = 80;
short	LINES = 24;

struct	option options[NOPTS + 1] = {
	"autoindent",	"ai",	ONOFF,		0,	0,	0,
	"autoprint",	"ap",	ONOFF,		1,	1,	0,
	"beautify",	"bf",	ONOFF,		0,	0,	0,
	"directory",	"dir",	STRING,		0,	0,	direct,
	"errorbells",	"eb",	ONOFF,		0,	0,	0,
	"ignorecase",	"ic",	ONOFF,		0,	0,	0,
#ifdef LISP
	"lisp",		0,	ONOFF,		0,	0,	0,
#endif
	"list",		0,	ONOFF,		0,	0,	0,
	"magic",	0,	ONOFF,		1,	1,	0,
	"number",	"nu",	ONOFF,		0,	0,	0,
	"open",		0,	ONOFF,		1,	1,	0,
	"optimize",	"opt",	ONOFF,		0,	0,	0,
	"paragraphs",	"para",	STRING,		0,	0,	paragraphs,
	"prompt",	0,	ONOFF,		1,	1,	0,
	"redraw",	0,	ONOFF,		0,	0,	0,
	"report",	0,	NUMERIC,	5,	5,	0,
	"scroll",	"scr",	NUMERIC,	12,	12,	0,
	"sections",	"sect",	STRING,		0,	0,	sections,
	"shell",	"sh",	STRING,		0,	0,	shell,
	"shiftwidth",	"sw",	NUMERIC,	TABS,	TABS,	0,
#ifdef LISP
	"showmatch",	"sm",	ONOFF,		0,	0,	0,
#endif
	"slowopen",	"slow",	ONOFF,		0,	0,	0,
	"tabstop",	"ts",	NUMERIC,	TABS,	TABS,	0,
	"ttytype",	"tty",	OTERM,		0,	0,	ttytype,
	"term",		0,	OTERM,		0,	0,	ttytype,
	"terse",	0,	ONOFF,		0,	0,	0,
	"warn",		0,	ONOFF,		1,	1,	0,
	"window",	"wi",	NUMERIC,	23,	23,	0,
	"wrapscan",	"ws",	ONOFF,		1,	1,	0,
	"wrapmargin",	"wm",	NUMERIC,	0,	0,	0,
	"writeany",	"wa",	ONOFF,		0,	0,	0,
	0,		0,	0,		0,	0,	0,
};
