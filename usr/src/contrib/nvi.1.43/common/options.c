/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)options.c	9.7 (Berkeley) 12/2/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <bitstring.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#include "compat.h"
#include <db.h>
#include <regex.h>
#include <pathnames.h>

#include "vi.h"
#include "excmd.h"

static int	 	 opts_abbcmp __P((const void *, const void *));
static int	 	 opts_cmp __P((const void *, const void *));
static OPTLIST const	*opts_prefix __P((char *));
static int	 	 opts_print __P((SCR *, OPTLIST const *));

/*
 * O'Reilly noted options and abbreviations are from "Learning the VI Editor",
 * Fifth Edition, May 1992.  There's no way of knowing what systems they are
 * actually from.
 *
 * HPUX noted options and abbreviations are from "The Ultimate Guide to the
 * VI and EX Text Editors", 1990.
 */
static OPTLIST const optlist[] = {
/* O_ALTWERASE	  4.4BSD */
	{"altwerase",	f_altwerase,	OPT_0BOOL,	0},
/* O_AUTOINDENT	    4BSD */
	{"autoindent",	NULL,		OPT_0BOOL,	0},
/* O_AUTOPRINT	    4BSD */
	{"autoprint",	NULL,		OPT_1BOOL,	0},
/* O_AUTOWRITE	    4BSD */
	{"autowrite",	NULL,		OPT_0BOOL,	0},
/* O_BACKUP	  4.4BSD */
	{"backup",	NULL,		OPT_STR,	0},
/* O_BEAUTIFY	    4BSD */
	{"beautify",	NULL,		OPT_0BOOL,	0},
/* O_CDPATH	  4.4BSD */
	{"cdpath",	f_cdpath,	OPT_STR,	0},
/* O_COLUMNS	  4.4BSD */
	{"columns",	f_columns,	OPT_NUM,	OPT_NOSAVE},
/* O_COMMENT	  4.4BSD */
	{"comment",	NULL,		OPT_0BOOL,	0},
/* O_DIGRAPH	  XXX: Elvis */
	{"digraph",	NULL,		OPT_0BOOL,	0},
/* O_DIRECTORY	    4BSD */
	{"directory",	NULL,		OPT_STR,	0},
/* O_EDCOMPATIBLE   4BSD */
	{"edcompatible",NULL,		OPT_0BOOL,	0},
/* O_ERRORBELLS	    4BSD */
	{"errorbells",	NULL,		OPT_0BOOL,	0},
/* O_EXRC	System V (undocumented) */
	{"exrc",	NULL,		OPT_0BOOL,	0},
/* O_EXTENDED	  4.4BSD */
	{"extended",	NULL,		OPT_0BOOL,	0},
/* O_FLASH	    HPUX */
	{"flash",	NULL,		OPT_1BOOL,	0},
/* O_HARDTABS	    4BSD */
	{"hardtabs",	NULL,		OPT_NUM,	0},
/* O_IGNORECASE	    4BSD */
	{"ignorecase",	NULL,		OPT_0BOOL,	0},
/* O_KEYTIME	  4.4BSD */
	{"keytime",	NULL,		OPT_NUM,	0},
/* O_LEFTRIGHT	  4.4BSD */
	{"leftright",	f_leftright,	OPT_0BOOL,	0},
/* O_LINES	  4.4BSD */
	{"lines",	f_lines,	OPT_NUM,	OPT_NOSAVE},
/* O_LISP	    4BSD */
/*
 * XXX
 * When the lisp option is implemented, delete the OPT_NOSAVE flag,
 * so that :mkexrc dumps it.
 */
	{"lisp",	f_lisp,		OPT_0BOOL,	OPT_NOSAVE},
/* O_LIST	    4BSD */
	{"list",	f_list,		OPT_0BOOL,	0},
/*
 * XXX
 * Locking isn't reliable enough over NFS to require it, in addition,
 * it's a serious startup performance problem over some remote links.
 */
/* O_LOCK	  4.4BSD */
	{"lock",	NULL,		OPT_1BOOL,	0},
/* O_MAGIC	    4BSD */
	{"magic",	NULL,		OPT_1BOOL,	0},
/* O_MATCHTIME	  4.4BSD */
	{"matchtime",	NULL,		OPT_NUM,	0},
/* O_MESG	    4BSD */
	{"mesg",	f_mesg,		OPT_1BOOL,	0},
/* O_MODELINE	    4BSD */
	{"modeline",	f_modeline,	OPT_0BOOL,	0},
/* O_MSGCAT	  4.4BSD */
	{"msgcat",	f_msgcat,	OPT_STR,	0},
/* O_NUMBER	    4BSD */
	{"number",	f_number,	OPT_0BOOL,	0},
/* O_OCTAL	  4.4BSD */
	{"octal",	f_octal,	OPT_0BOOL,	0},
/* O_OPEN	    4BSD */
	{"open",	NULL,		OPT_1BOOL,	0},
/* O_OPTIMIZE	    4BSD */
	{"optimize",	NULL,		OPT_1BOOL,	0},
/* O_PARAGRAPHS	    4BSD */
	{"paragraphs",	f_paragraph,	OPT_STR,	0},
/* O_PROMPT	    4BSD */
	{"prompt",	NULL,		OPT_1BOOL,	0},
/* O_READONLY	    4BSD (undocumented) */
	{"readonly",	f_readonly,	OPT_0BOOL,	0},
/* O_RECDIR	  4.4BSD */
	{"recdir",	NULL,		OPT_STR,	0},
/* O_REDRAW	    4BSD */
	{"redraw",	NULL,		OPT_0BOOL,	0},
/* O_REMAP	    4BSD */
	{"remap",	NULL,		OPT_1BOOL,	0},
/* O_REPORT	    4BSD */
	{"report",	NULL,		OPT_NUM,	0},
/* O_RULER	  4.4BSD */
	{"ruler",	NULL,		OPT_0BOOL,	0},
/* O_SCROLL	    4BSD */
	{"scroll",	NULL,		OPT_NUM,	0},
/* O_SECTIONS	    4BSD */
	{"sections",	f_section,	OPT_STR,	0},
/* O_SHELL	    4BSD */
	{"shell",	NULL,		OPT_STR,	0},
/* O_SHELLMETA	  4.4BSD */
	{"shellmeta",	NULL,		OPT_STR,	0},
/* O_SHIFTWIDTH	    4BSD */
	{"shiftwidth",	f_shiftwidth,	OPT_NUM,	0},
/* O_SHOWMATCH	    4BSD */
	{"showmatch",	NULL,		OPT_0BOOL,	0},
/* O_SHOWMODE	  4.4BSD */
	{"showmode",	NULL,		OPT_0BOOL,	0},
/* O_SIDESCROLL	  4.4BSD */
	{"sidescroll",	NULL,		OPT_NUM,	0},
/* O_SLOWOPEN	    4BSD  */
	{"slowopen",	NULL,		OPT_0BOOL,	0},
/* O_SOURCEANY	    4BSD (undocumented) */
	{"sourceany",	f_sourceany,	OPT_0BOOL,	0},
/* O_TABSTOP	    4BSD */
	{"tabstop",	f_tabstop,	OPT_NUM,	0},
/* O_TAGLENGTH	    4BSD */
	{"taglength",	NULL,		OPT_NUM,	0},
/* O_TAGS	    4BSD */
	{"tags",	f_tags,		OPT_STR,	0},
/*
 * !!!
 * By default, the historic vi always displayed information about two
 * options, redraw and term.  Term seems sufficient.
 */
/* O_TERM	    4BSD */
	{"term",	f_term,		OPT_STR,	OPT_ADISP|OPT_NOSAVE},
/* O_TERSE	    4BSD */
	{"terse",	NULL,		OPT_0BOOL,	0},
/* O_TILDEOP      4.4BSD */
	{"tildeop",	NULL,		OPT_0BOOL,	0},
/* O_TIMEOUT	    4BSD (undocumented) */
	{"timeout",	NULL,		OPT_1BOOL,	0},
/* O_TTYWERASE	  4.4BSD */
	{"ttywerase",	f_ttywerase,	OPT_0BOOL,	0},
/* O_VERBOSE	  4.4BSD */
	{"verbose",	NULL,		OPT_0BOOL,	0},
/* O_W1200	    4BSD */
	{"w1200",	f_w1200,	OPT_NUM,	OPT_NDISP|OPT_NOSAVE},
/* O_W300	    4BSD */
	{"w300",	f_w300,		OPT_NUM,	OPT_NDISP|OPT_NOSAVE},
/* O_W9600	    4BSD */
	{"w9600",	f_w9600,	OPT_NUM,	OPT_NDISP|OPT_NOSAVE},
/* O_WARN	    4BSD */
	{"warn",	NULL,		OPT_1BOOL,	0},
/* O_WINDOW	    4BSD */
	{"window",	f_window,	OPT_NUM,	0},
/* O_WRAPLEN	  4.4BSD */
	{"wraplen",	NULL,		OPT_NUM,	0},
/* O_WRAPMARGIN	    4BSD */
	{"wrapmargin",	NULL,		OPT_NUM,	0},
/* O_WRAPSCAN	    4BSD */
	{"wrapscan",	NULL,		OPT_1BOOL,	0},
/* O_WRITEANY	    4BSD */
	{"writeany",	NULL,		OPT_0BOOL,	0},
	{NULL},
};

typedef struct abbrev {
        char *name;
        int offset;
} OABBREV;

static OABBREV const abbrev[] = {
	{"ai",		O_AUTOINDENT},		/*     4BSD */
	{"ap",		O_AUTOPRINT},		/*     4BSD */
	{"aw",		O_AUTOWRITE},		/*     4BSD */
	{"bf",		O_BEAUTIFY},		/*     4BSD */
	{"co",		O_COLUMNS},		/*   4.4BSD */
	{"dir",		O_DIRECTORY},		/*     4BSD */
	{"eb",		O_ERRORBELLS},		/*     4BSD */
	{"ed",		O_EDCOMPATIBLE},	/*     4BSD */
	{"ex",		O_EXRC},		/* System V (undocumented) */
	{"ht",		O_HARDTABS},		/*     4BSD */
	{"ic",		O_IGNORECASE},		/*     4BSD */
	{"li",		O_LINES},		/*   4.4BSD */
	{"modelines",	O_MODELINE},		/*     HPUX */
	{"nu",		O_NUMBER},		/*     4BSD */
	{"opt",		O_OPTIMIZE},		/*     4BSD */
	{"para",	O_PARAGRAPHS},		/*     4BSD */
	{"re",		O_REDRAW},		/* O'Reilly */
	{"ro",		O_READONLY},		/*     4BSD (undocumented) */
	{"scr",		O_SCROLL},		/*     4BSD (undocumented) */
	{"sect",	O_SECTIONS},		/* O'Reilly */
	{"sh",		O_SHELL},		/*     4BSD */
	{"slow",	O_SLOWOPEN},		/*     4BSD */
	{"sm",		O_SHOWMATCH},		/*     4BSD */
	{"sw",		O_SHIFTWIDTH},		/*     4BSD */
	{"tag",		O_TAGS},		/*     4BSD (undocumented) */
	{"tl",		O_TAGLENGTH},		/*     4BSD */
	{"to",		O_TIMEOUT},		/*     4BSD (undocumented) */
	{"ts",		O_TABSTOP},		/*     4BSD */
	{"tty",		O_TERM},		/*     4BSD (undocumented) */
	{"ttytype",	O_TERM},		/*     4BSD (undocumented) */
	{"w",		O_WINDOW},		/* O'Reilly */
	{"wa",		O_WRITEANY},		/*     4BSD */
	{"wi",		O_WINDOW},		/*     4BSD (undocumented) */
	{"wl",		O_WRAPLEN},		/*   4.4BSD */
	{"wm",		O_WRAPMARGIN},		/*     4BSD */
	{"ws",		O_WRAPSCAN},		/*     4BSD */
	{NULL},
};

/*
 * opts_init --
 *	Initialize some of the options.
 */
int
opts_init(sp, oargs)
	SCR *sp;
	int *oargs;
{
	ARGS *argv[2], a, b;
	OPTLIST const *op;
	u_long v;
	int cnt;
	char *s, b1[1024];

	a.bp = b1;
	a.len = 0;
	b.bp = NULL;
	b.len = 0;
	argv[0] = &a;
	argv[1] = &b;

	/* Set boolean default values. */
	for (op = optlist, cnt = 0; op->name != NULL; ++op, ++cnt)
		switch (op->type) {
		case OPT_0BOOL:
			O_CLR(sp, cnt);
			O_D_CLR(sp, cnt);
			break;
		case OPT_1BOOL:
			O_SET(sp, cnt);
			O_D_SET(sp, cnt);
			break;
		case OPT_NUM:
		case OPT_STR:
			break;
		default:
			abort();
		}

	/* Set numeric and string default values. */
#define	OI(opt, str, setdef) {						\
	if (str != b1)		/* GCC puts strings in text-space. */	\
		(void)strcpy(b1, str);					\
	a.len = strlen(b1);						\
	if (opts_set(sp, argv, setdef, NULL)) {				\
		msgq(sp, M_ERR,						\
		    "052|Unable to set default %s option",		\
		    optlist[opt]);					\
		return (1);						\
	}								\
}
	OI(O_BACKUP, "backup=", 1);
	(void)snprintf(b1, sizeof(b1), "cdpath=%s",
	    (s = getenv("CDPATH")) == NULL ? ":" : s);
	OI(O_CDPATH, b1, 1);

	/*
	 * !!!
	 * Vi historically stored temporary files in /var/tmp.  We store them
	 * in /tmp by default, hoping it's a memory based file system.  There
	 * are two ways to change this -- the user can set either the directory
	 * option or the TMPDIR environmental variable.
	 */
	(void)snprintf(b1, sizeof(b1), "directory=%s",
	    (s = getenv("TMPDIR")) == NULL ? _PATH_TMP : s);
	OI(O_DIRECTORY, b1, 1);
	OI(O_KEYTIME, "keytime=6", 1);
	OI(O_MATCHTIME, "matchtime=7", 1);
	(void)snprintf(b1, sizeof(b1), "msgcat=%s", _PATH_MSGCAT);
	OI(O_MSGCAT, b1, 1);
	OI(O_REPORT, "report=5", 1);
	OI(O_PARAGRAPHS, "paragraphs=IPLPPPQPP LIpplpipbp", 1);
	(void)snprintf(b1, sizeof(b1), "recdir=%s", _PATH_PRESERVE);
	OI(O_RECDIR, b1, 1);
	OI(O_SECTIONS, "sections=NHSHH HUnhsh", 1);
	(void)snprintf(b1, sizeof(b1), "shell=%s",
	    (s = getenv("SHELL")) == NULL ? _PATH_BSHELL : s);
	OI(O_SHELL, b1, 1);
	OI(O_SHELLMETA, "shellmeta=~{[*?$`'\"\\", 1);
	OI(O_SHIFTWIDTH, "shiftwidth=8", 1);
	OI(O_SIDESCROLL, "sidescroll=16", 1);
	OI(O_TABSTOP, "tabstop=8", 1);
	(void)snprintf(b1, sizeof(b1), "tags=%s", _PATH_TAGS);
	OI(O_TAGS, b1, 1);
	(void)snprintf(b1, sizeof(b1), "term=%s",
	    (s = getenv("TERM")) == NULL ? "unknown" : s);
	OI(O_TERM, b1, 1);

	/*
	 * XXX
	 * Initialize O_SCROLL here, after term; initializing term should
	 * have created a LINES/COLUMNS value.
	 */
	(void)snprintf(b1, sizeof(b1),
	    "scroll=%ld", (O_VAL(sp, O_LINES) - 1) / 2);
	OI(O_SCROLL, b1, 1);

	/*
	 * The default window option values are:
	 *		8 if baud rate <=  600
	 *	       16 if baud rate <= 1200
	 *	LINES - 1 if baud rate  > 1200
	 */
	v = baud_from_bval(sp);
	if (v <= 600)
		v = 8;
	else if (v <= 1200)
		v = 16;
	else
		v = O_VAL(sp, O_LINES) - 1;
	(void)snprintf(b1, sizeof(b1), "window=%lu", v);
	OI(O_WINDOW, b1, 1);

	/*
	 * !!!
	 * Some options can be initialized by the command name or the
	 * command-line arguments.  They don't set the default values,
	 * it's historic practice.
	 */
	for (; *oargs != -1; ++oargs)
		OI(*oargs, optlist[*oargs].name, 0);
#undef OI

	return (0);
}

/*
 * opts_set --
 *	Change the values of one or more options.
 */
int
opts_set(sp, argv, setdef, usage)
	SCR *sp;
	ARGS *argv[];
	int setdef;
	char *usage;
{
	enum optdisp disp;
	enum nresult nret;
	OABBREV atmp, *ap;
	OPTLIST const *op;
	OPTLIST otmp;
	OPTION *spo;
	u_long value, turnoff;
	int ch, equals, nf, offset, qmark, rval;
	char *endp, *name, *p, *sep;

	disp = NO_DISPLAY;
	for (rval = 0; argv[0]->len != 0; ++argv) {
		/*
		 * The historic vi dumped the options for each occurrence of
		 * "all" in the set list.  Puhleeze.
		 */
		if (!strcmp(argv[0]->bp, "all")) {
			disp = ALL_DISPLAY;
			continue;
		}

		/* Find equals sign or question mark. */
		for (sep = NULL, equals = qmark = 0,
		    p = name = argv[0]->bp; (ch = *p) != '\0'; ++p)
			if (ch == '=' || ch == '?') {
				if (p == name) {
					if (usage != NULL)
						msgq(sp, M_ERR,
						    "053|Usage: %s", usage);
					return (1);
				}
				sep = p;
				if (ch == '=')
					equals = 1;
				else
					qmark = 1;
				break;
			}

		turnoff = 0;
		op = NULL;
		if (sep != NULL)
			*sep++ = '\0';

		/* Check list of abbreviations. */
		atmp.name = name;
		if ((ap = bsearch(&atmp, abbrev,
		    sizeof(abbrev) / sizeof(OABBREV) - 1,
		    sizeof(OABBREV), opts_abbcmp)) != NULL) {
			op = optlist + ap->offset;
			goto found;
		}

		/* Check list of options. */
		otmp.name = name;
		if ((op = bsearch(&otmp, optlist,
		    sizeof(optlist) / sizeof(OPTLIST) - 1,
		    sizeof(OPTLIST), opts_cmp)) != NULL)
			goto found;

		/* Try the name without any leading "no". */
		if (name[0] == 'n' && name[1] == 'o') {
			turnoff = 1;
			name += 2;
		} else
			goto prefix;

		/* Check list of abbreviations. */
		atmp.name = name;
		if ((ap = bsearch(&atmp, abbrev,
		    sizeof(abbrev) / sizeof(OABBREV) - 1,
		    sizeof(OABBREV), opts_abbcmp)) != NULL) {
			op = optlist + ap->offset;
			goto found;
		}

		/* Check list of options. */
		otmp.name = name;
		if ((op = bsearch(&otmp, optlist,
		    sizeof(optlist) / sizeof(OPTLIST) - 1,
		    sizeof(OPTLIST), opts_cmp)) != NULL)
			goto found;

		/* Check for prefix match. */
prefix:		op = opts_prefix(name);

found:		if (op == NULL) {
			p = msg_print(sp, name, &nf);
			msgq(sp, M_ERR,
		    "054|no %s option: 'set all' gives all option values",
			    p);
			if (nf)
				FREE_SPACE(sp, p, 0);
			continue;
		}

		/* Find current option values. */
		offset = op - optlist;
		spo = sp->opts + offset;

		/*
		 * !!!
		 * Historically, the question mark could be a separate
		 * argument.
		 */
		if (!equals && !qmark &&
		    argv[1]->len == 1 && argv[1]->bp[0] == '?') {
			++argv;
			qmark = 1;
		}

		/* Set name, value. */
		switch (op->type) {
		case OPT_0BOOL:
		case OPT_1BOOL:
			if (equals) {
				p = msg_print(sp, name, &nf);
				msgq(sp, M_ERR,
			    "055|set: [no]%s option doesn't take a value", p);
				if (nf)
					FREE_SPACE(sp, p, 0);
				break;
			}
			if (qmark) {
				if (!disp)
					disp = SELECT_DISPLAY;
				F_SET(spo, OPT_SELECTED);
				break;
			}
			if (op->func != NULL) {
				if (op->func(sp, spo, NULL, turnoff)) {
					rval = 1;
					break;
				}
			} else if (turnoff)
				O_CLR(sp, offset);
			else
				O_SET(sp, offset);
			if (setdef)
				O_D_VAL(sp, offset) = O_VAL(sp, offset);
			goto change;
		case OPT_NUM:
			if (turnoff) {
				p = msg_print(sp, name, &nf);
				msgq(sp, M_ERR,
				    "056|set: %s option isn't a boolean", p);
				if (nf)
					FREE_SPACE(sp, p, 0);
				break;
			}
			if (qmark || !equals) {
				if (!disp)
					disp = SELECT_DISPLAY;
				F_SET(spo, OPT_SELECTED);
				break;
			}

			if (!isdigit(sep[0]))
				goto badnum;
			if ((nret = nget_uslong(sp,
			    &value, sep, &endp, 10)) != NUM_OK) {
				p = msg_print(sp, sep, &nf);
				switch (nret) {
				case NUM_ERR:
					msgq(sp, M_SYSERR, "%s", p);
					break;
				case NUM_OVER:
					msgq(sp, M_ERR,
					    "XXX|%s: option value overflow", p);
					break;
				case NUM_UNDER:
					abort();
					/* NOTREACHED */
				}
				if (nf)
					FREE_SPACE(sp, p, 0);
				break;
			}
			if (*endp && !isblank(*endp)) {
badnum:				p = msg_print(sp, sep, &nf);
				msgq(sp, M_ERR,
				    "057|set: illegal number %s", p);
				if (nf)
					FREE_SPACE(sp, p, 0);
				break;
			}
			if (op->func != NULL) {
				if (op->func(sp, spo, sep, value)) {
					rval = 1;
					break;
				}
			} else
				O_VAL(sp, offset) = value;
			if (setdef)
				O_D_VAL(sp, offset) = O_VAL(sp, offset);
			goto change;
		case OPT_STR:
			if (turnoff) {
				p = msg_print(sp, name, &nf);
				msgq(sp, M_ERR,
				    "058|set: %s option isn't a boolean", p);
				if (nf)
					FREE_SPACE(sp, p, 0);
				break;
			}
			if (qmark || !equals) {
				if (!disp)
					disp = SELECT_DISPLAY;
				F_SET(spo, OPT_SELECTED);
				break;
			}
			if (op->func != NULL) {
				if (op->func(sp, spo, sep, (u_long)0)) {
					rval = 1;
					break;
				}
			} else {
				if (setdef && O_D_STR(sp, offset) != NULL) {
					free(O_D_STR(sp, offset));
					O_D_STR(sp, offset) = NULL;
				}
				if (O_STR(sp, offset) != NULL &&
				    O_STR(sp, offset) != O_D_STR(sp, offset))
					free(O_STR(sp, offset));
				if ((O_STR(sp, offset) = strdup(sep)) == NULL) {
					msgq(sp, M_SYSERR, NULL);
					rval = 1;
					break;
				}
			}
			if (setdef)
				O_D_STR(sp, offset) = O_STR(sp, offset);
			/* Give underlying functions a chance. */
change:			(void)ex_optchange(sp, offset);
			(void)sex_optchange(sp, offset);
			(void)svi_optchange(sp, offset);
			(void)v_optchange(sp, offset);
			break;
		default:
			abort();
		}
	}
	if (disp != NO_DISPLAY)
		opts_dump(sp, disp);
	return (rval);
}

/*
 * opts_dump --
 *	List the current values of selected options.
 */
void
opts_dump(sp, type)
	SCR *sp;
	enum optdisp type;
{
	OPTLIST const *op;
	int base, b_num, cnt, col, colwidth, curlen, s_num;
	int numcols, numrows, row;
	int b_op[O_OPTIONCOUNT], s_op[O_OPTIONCOUNT];
	char nbuf[20];

	/*
	 * Options are output in two groups -- those that fit in a column and
	 * those that don't.  Output is done on 6 character "tab" boundaries
	 * for no particular reason.  (Since we don't output tab characters,
	 * we can ignore the terminal's tab settings.)  Ignore the user's tab
	 * setting because we have no idea how reasonable it is.
	 *
	 * Find a column width we can live with.
	 */
	for (cnt = 6; cnt > 1; --cnt) {
		colwidth = (sp->cols - 1) / cnt & ~(STANDARD_TAB - 1);
		if (colwidth >= 10) {
			colwidth =
			    (colwidth + STANDARD_TAB) & ~(STANDARD_TAB - 1);
			break;
		}
		colwidth = 0;
	}

	/*
	 * Get the set of options to list, entering them into
	 * the column list or the overflow list.
	 */
	for (b_num = s_num = 0, op = optlist; op->name != NULL; ++op) {
		cnt = op - optlist;

		/* If OPT_NDISP set, it's never displayed. */
		if (F_ISSET(op, OPT_NDISP))
			continue;

		switch (type) {
		case ALL_DISPLAY:		/* Display all. */
			break;
		case CHANGED_DISPLAY:		/* Display changed. */
			/* If OPT_ADISP set, it's always "changed". */
			if (F_ISSET(op, OPT_ADISP))
				break;
			switch (op->type) {
			case OPT_0BOOL:
			case OPT_1BOOL:
			case OPT_NUM:
				if (O_VAL(sp, cnt) == O_D_VAL(sp, cnt))
					continue;
				break;
			case OPT_STR:
				if (O_STR(sp, cnt) == O_D_STR(sp, cnt))
					continue;
				if (!strcmp(O_STR(sp, cnt), O_D_STR(sp, cnt)))
					continue;
				break;
			}
			break;
		case SELECT_DISPLAY:		/* Display selected. */
			if (!F_ISSET(&sp->opts[cnt], OPT_SELECTED))
				continue;
			break;
		default:
		case NO_DISPLAY:
			abort();
			/* NOTREACHED */
		}
		F_CLR(&sp->opts[cnt], OPT_SELECTED);

		curlen = strlen(op->name);
		switch (op->type) {
		case OPT_0BOOL:
		case OPT_1BOOL:
			if (!O_ISSET(sp, cnt))
				curlen += 2;
			break;
		case OPT_NUM:
			(void)snprintf(nbuf,
			    sizeof(nbuf), "%ld", O_VAL(sp, cnt));
			curlen += strlen(nbuf);
			break;
		case OPT_STR:
			curlen += strlen(O_STR(sp, cnt)) + 3;
			break;
		}
		/* Offset by two so there's a gap. */
		if (curlen < colwidth - 2)
			s_op[s_num++] = cnt;
		else
			b_op[b_num++] = cnt;
	}

	if (s_num > 0) {
		/* Figure out the number of columns. */
		numcols = (sp->cols - 1) / colwidth;
		if (s_num > numcols) {
			numrows = s_num / numcols;
			if (s_num % numcols)
				++numrows;
		} else
			numrows = 1;

		/* Display the options in sorted order. */
		for (row = 0; row < numrows;) {
			for (base = row, col = 0; col < numcols; ++col) {
				cnt = opts_print(sp, &optlist[s_op[base]]);
				if ((base += numrows) >= s_num)
					break;
				(void)ex_printf(EXCOOKIE,
				    "%*s", (int)(colwidth - cnt), "");
			}
			if (++row < numrows || b_num)
				(void)ex_printf(EXCOOKIE, "\n");
		}
	}

	for (row = 0; row < b_num;) {
		(void)opts_print(sp, &optlist[b_op[row]]);
		if (++row < b_num)
			(void)ex_printf(EXCOOKIE, "\n");
	}
	F_SET(sp, S_SCR_EXWROTE);
	(void)ex_printf(EXCOOKIE, "\n");
}

/*
 * opts_print --
 *	Print out an option.
 */
static int
opts_print(sp, op)
	SCR *sp;
	OPTLIST const *op;
{
	int curlen, offset;

	curlen = 0;
	offset = op - optlist;
	switch (op->type) {
	case OPT_0BOOL:
	case OPT_1BOOL:
		curlen += ex_printf(EXCOOKIE,
		    "%s%s", O_ISSET(sp, offset) ? "" : "no", op->name);
		break;
	case OPT_NUM:
		curlen += ex_printf(EXCOOKIE,
		     "%s=%ld", op->name, O_VAL(sp, offset));
		break;
	case OPT_STR:
		curlen += ex_printf(EXCOOKIE,
		    "%s=\"%s\"", op->name, O_STR(sp, offset));
		break;
	}
	return (curlen);
}

/*
 * opts_save --
 *	Write the current configuration to a file.
 */
int
opts_save(sp, fp)
	SCR *sp;
	FILE *fp;
{
	OPTLIST const *op;
	int ch, cnt;
	char *p;

	for (op = optlist; op->name != NULL; ++op) {
		if (F_ISSET(op, OPT_NOSAVE))
			continue;
		cnt = op - optlist;
		switch (op->type) {
		case OPT_0BOOL:
		case OPT_1BOOL:
			if (O_ISSET(sp, cnt))
				(void)fprintf(fp, "set %s\n", op->name);
			else
				(void)fprintf(fp, "set no%s\n", op->name);
			break;
		case OPT_NUM:
			(void)fprintf(fp,
			    "set %s=%-3d\n", op->name, O_VAL(sp, cnt));
			break;
		case OPT_STR:
			(void)fprintf(fp, "set ");
			for (p = op->name; (ch = *p) != '\0'; ++p) {
				if (isblank(ch) || ch == '\\')
					(void)putc('\\', fp);
				(void)putc(ch, fp);
			}
			(void)putc('=', fp);
			for (p = O_STR(sp, cnt); (ch = *p) != '\0'; ++p) {
				if (isblank(ch) || ch == '\\')
					(void)putc('\\', fp);
				(void)putc(ch, fp);
			}
			(void)putc('\n', fp);
			break;
		}
		if (ferror(fp)) {
			msgq(sp, M_SYSERR, NULL);
			return (1);
		}
	}
	return (0);
}

/*
 * opts_prefix --
 *	Check to see if the name is the prefix of one (and only one)
 *	option.  If so, return the option.
 */
static OPTLIST const *
opts_prefix(name)
	char *name;
{
	OPTLIST const *op, *save_op;
	size_t len;

	save_op = NULL;
	len = strlen(name);
	for (op = optlist; op->name != NULL; ++op) {
		if (op->name[0] < name[0])
			continue;
		if (op->name[0] > name[0])
			break;
		if (!memcmp(op->name, name, len)) {
			if (save_op != NULL)
				return (NULL);
			save_op = op;
		}
	}
	return (save_op);
}

static int
opts_abbcmp(a, b)
        const void *a, *b;
{
        return(strcmp(((OABBREV *)a)->name, ((OABBREV *)b)->name));
}

static int
opts_cmp(a, b)
        const void *a, *b;
{
        return(strcmp(((OPTLIST *)a)->name, ((OPTLIST *)b)->name));
}

/*
 * opts_free --
 *	Free all option strings
 */
void
opts_free(sp)
	SCR *sp;
{
	int cnt;

	for (cnt = 0; cnt < O_OPTIONCOUNT; ++cnt) {
		if (optlist[cnt].type != OPT_STR)
			continue;
		/*
		 * The default and current strings may reference the same
		 * memory.
		 */
		if (O_STR(sp, cnt) != O_D_STR(sp, cnt)) {
			if (O_D_STR(sp, cnt) != NULL)
				free(O_D_STR(sp, cnt));
		}
		if (O_STR(sp, cnt) != NULL)
			free(O_STR(sp, cnt));
	}
}

/*
 * opts_copy --
 *	Copy a screen's OPTION array.
 */
int
opts_copy(orig, sp)
	SCR *orig, *sp;
{
	int cnt, rval;

	/* Copy most everything without change. */
	memmove(sp->opts, orig->opts, sizeof(orig->opts));

	/* Copy the string edit options. */
	for (cnt = rval = 0; cnt < O_OPTIONCOUNT; ++cnt) {
		if (optlist[cnt].type != OPT_STR)
			continue;
		/*
		 * If already failed, just NULL out the entries -- have to
		 * continue, otherwise would have two screens referencing
		 * the same memory.
		 */
		if (rval) {
			O_STR(sp, cnt) = O_D_STR(sp, cnt) = NULL;
			continue;
		}
		/* Copy the current string. */
		if ((O_STR(sp, cnt) = strdup(O_STR(sp, cnt))) == NULL) {
			O_D_STR(sp, cnt) = NULL;
			goto nomem;
		}
		/* If the default was the same as the current, repoint it. */
		if (O_D_STR(orig, cnt) == O_STR(orig, cnt)) {
			O_D_STR(sp, cnt) = O_STR(sp, cnt);
			continue;
		}
		/* Copy the default string. */
		if ((O_D_STR(sp, cnt) = strdup(O_D_STR(sp, cnt))) == NULL) {
nomem:			msgq(orig, M_SYSERR, NULL);
			rval = 1;
		}
	}
	return (rval);
}
