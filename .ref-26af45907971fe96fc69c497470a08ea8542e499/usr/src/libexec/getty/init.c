/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)init.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Getty table initializations.
 *
 * Melbourne getty.
 */
#include <sgtty.h>
#include "gettytab.h"
#include "pathnames.h"

extern	struct sgttyb tmode;
extern	struct tchars tc;
extern	struct ltchars ltc;
extern	char hostname[];

struct	gettystrs gettystrs[] = {
	{ "nx" },			/* next table */
	{ "cl" },			/* screen clear characters */
	{ "im" },			/* initial message */
	{ "lm", "login: " },		/* login message */
	{ "er", &tmode.sg_erase },	/* erase character */
	{ "kl", &tmode.sg_kill },	/* kill character */
	{ "et", &tc.t_eofc },		/* eof chatacter (eot) */
	{ "pc", "" },			/* pad character */
	{ "tt" },			/* terminal type */
	{ "ev" },			/* enviroment */
	{ "lo", _PATH_LOGIN },		/* login program */
	{ "hn", hostname },		/* host name */
	{ "he" },			/* host name edit */
	{ "in", &tc.t_intrc },		/* interrupt char */
	{ "qu", &tc.t_quitc },		/* quit char */
	{ "xn", &tc.t_startc },		/* XON (start) char */
	{ "xf", &tc.t_stopc },		/* XOFF (stop) char */
	{ "bk", &tc.t_brkc },		/* brk char (alt \n) */
	{ "su", &ltc.t_suspc },		/* suspend char */
	{ "ds", &ltc.t_dsuspc },	/* delayed suspend */
	{ "rp", &ltc.t_rprntc },	/* reprint char */
	{ "fl", &ltc.t_flushc },	/* flush output */
	{ "we", &ltc.t_werasc },	/* word erase */
	{ "ln", &ltc.t_lnextc },	/* literal next */
	{ 0 }
};

struct	gettynums gettynums[] = {
	{ "is" },			/* input speed */
	{ "os" },			/* output speed */
	{ "sp" },			/* both speeds */
	{ "nd" },			/* newline delay */
	{ "cd" },			/* carriage-return delay */
	{ "td" },			/* tab delay */
	{ "fd" },			/* form-feed delay */
	{ "bd" },			/* backspace delay */
	{ "to" },			/* timeout */
	{ "f0" },			/* output flags */
	{ "f1" },			/* input flags */
	{ "f2" },			/* user mode flags */
	{ "pf" },			/* delay before flush at 1st prompt */
	{ 0 }
};

struct	gettyflags gettyflags[] = {
	{ "ht",	0 },			/* has tabs */
	{ "nl",	1 },			/* has newline char */
	{ "ep",	0 },			/* even parity */
	{ "op",	0 },			/* odd parity */
	{ "ap",	0 },			/* any parity */
	{ "ec",	1 },			/* no echo */
	{ "co",	0 },			/* console special */
	{ "cb",	0 },			/* crt backspace */
	{ "ck",	0 },			/* crt kill */
	{ "ce",	0 },			/* crt erase */
	{ "pe",	0 },			/* printer erase */
	{ "rw",	1 },			/* don't use raw */
	{ "xc",	1 },			/* don't ^X ctl chars */
	{ "lc",	0 },			/* terminal las lower case */
	{ "uc",	0 },			/* terminal has no lower case */
	{ "ig",	0 },			/* ignore garbage */
	{ "ps",	0 },			/* do port selector speed select */
	{ "hc",	1 },			/* don't set hangup on close */
	{ "ub", 0 },			/* unbuffered output */
	{ "ab", 0 },			/* auto-baud detect with '\r' */
	{ "dx", 0 },			/* set decctlq */
	{ "np", 0 },			/* no parity at all (8bit chars) */
	{ 0 }
};
