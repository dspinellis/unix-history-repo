/*-
 * Copyright (c) 1993, 1994
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
static char sccsid[] = "@(#)options_f.c	9.6 (Berkeley) 11/24/94";
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

#include "vi.h"
#include "../ex/tag.h"

static int	opt_dup __P((SCR *, int, char *));
static int	opt_putenv __P((char *));

#define	DECL(f)								\
	int								\
	f(sp, op, str, val)						\
		SCR *sp;						\
		OPTION *op;						\
		char *str;						\
		u_long val;
#define	CALL(f)								\
	f(sp, op, str, val)

#define	turnoff	val

DECL(f_altwerase)
{
	if (turnoff)
		O_CLR(sp, O_ALTWERASE);
	else {
		O_SET(sp, O_ALTWERASE);
		O_CLR(sp, O_TTYWERASE);
	}
	return (0);
}

DECL(f_cdpath)
{
	return (opt_dup(sp, O_CDPATH, str));
}

DECL(f_columns)
{
	char buf[25];

	/* Validate the number. */
	if (val < MINIMUM_SCREEN_COLS) {
		msgq(sp, M_ERR, "060|Screen columns too small, less than %d",
		    MINIMUM_SCREEN_COLS);
		return (1);
	}

	/*
	 * !!!
	 * It's not uncommon for allocation of huge chunks of memory to cause
	 * core dumps on various systems.  So, we prune out numbers that are
	 * "obviously" wrong.  Vi will not work correctly if it has the wrong
	 * number of lines/columns for the screen, but at least we don't drop
	 * core.
	 */
#define	MAXIMUM_SCREEN_COLS	500
	if (val > MAXIMUM_SCREEN_COLS) {
		msgq(sp, M_ERR, "266|Screen columns too large, greater than %d",
		    MAXIMUM_SCREEN_COLS);
		return (1);
	}

	/* Set the columns value in the environment for curses. */
	(void)snprintf(buf, sizeof(buf), "COLUMNS=%lu", val);
	if (opt_putenv(buf))
		return (1);

	/* This is expensive, don't do it unless it's necessary. */
	if (O_VAL(sp, O_COLUMNS) == val)
		return (0);

	/* Set the value. */
	O_VAL(sp, O_COLUMNS) =  val;

	F_SET(sp, S_SCR_RESIZE);
	return (0);
}

DECL(f_leftright)
{
	if (turnoff)
		O_CLR(sp, O_LEFTRIGHT);
	else
		O_SET(sp, O_LEFTRIGHT);
	F_SET(sp, S_SCR_REFORMAT);
	return (0);
}

DECL(f_lines)
{
	char buf[25];

	/* Validate the number. */
	if (val < MINIMUM_SCREEN_ROWS) {
		msgq(sp, M_ERR, "061|Screen lines too small, less than %d",
		    MINIMUM_SCREEN_ROWS);
		return (1);
	}

	/*
	 * !!!
	 * It's not uncommon for allocation of huge chunks of memory to cause
	 * core dumps on various systems.  So, we prune out numbers that are
	 * "obviously" wrong.  Vi will not work correctly if it has the wrong
	 * number of lines/columns for the screen, but at least we don't drop
	 * core.
	 */
#define	MAXIMUM_SCREEN_ROWS	500
	if (val > MAXIMUM_SCREEN_ROWS) {
		msgq(sp, M_ERR, "267|Screen lines too large, greater than %d",
		    MAXIMUM_SCREEN_ROWS);
		return (1);
	}

	/* Set the rows value in the environment for curses. */
	(void)snprintf(buf, sizeof(buf), "LINES=%lu", val);
	if (opt_putenv(buf))
		return (1);

	/* This is expensive, don't do it unless it's necessary. */
	if (O_VAL(sp, O_LINES) == val)
		return (0);

	/* Set the value, and the related scroll value. */
	O_VAL(sp, O_LINES) = val;
	O_VAL(sp, O_SCROLL) = (val - 1) / 2;

	/* If no window value set, set a new default window. */
	if (O_VAL(sp, O_WINDOW) == O_D_VAL(sp, O_WINDOW))
		O_VAL(sp, O_WINDOW) = O_D_VAL(sp, O_WINDOW) = val - 1;

	F_SET(sp, S_SCR_RESIZE);
	return (0);
}

DECL(f_lisp)
{
	msgq(sp, M_ERR, "062|The lisp option is not implemented");
	return (0);
}

DECL(f_list)
{
	if (turnoff)
		O_CLR(sp, O_LIST);
	else
		O_SET(sp, O_LIST);

	F_SET(sp, S_SCR_REFORMAT);
	return (0);
}

DECL(f_mesg)
{
	struct stat sb;
	char *tty;

	/* Find the tty. */
	if ((tty = ttyname(STDERR_FILENO)) == NULL) {
		msgq(sp, M_SYSERR, "stderr");
		return (1);
	}

	/* Save the tty mode for later; only save it once. */
	if (!F_ISSET(sp->gp, G_SETMODE)) {
		F_SET(sp->gp, G_SETMODE);
		if (stat(tty, &sb) < 0) {
			msgq(sp, M_SYSERR, "%s", tty);
			return (1);
		}
		sp->gp->origmode = sb.st_mode;
	}

	if (turnoff) {
		if (chmod(tty, sp->gp->origmode & ~S_IWGRP) < 0) {
			msgq(sp, M_SYSERR,
			    "063|messages not turned off: %s", tty);
			return (1);
		}
		O_CLR(sp, O_MESG);
	} else {
		if (chmod(tty, sp->gp->origmode | S_IWGRP) < 0) {
			msgq(sp, M_SYSERR,
			    "064|messages not turned on: %s", tty);
			return (1);
		}
		O_SET(sp, O_MESG);
	}
	return (0);
}

/*
 * f_modeline --
 *	This has been documented in historical systems as both "modeline"
 *	and as "modelines".  Regardless of the name, this option represents
 *	a security problem of mammoth proportions, not to mention a stunning
 *	example of what your intro CS professor referred to as the perils of
 *	mixing code and data.  Don't add it, or I will kill you.
 */
DECL(f_modeline)
{
	if (!turnoff)
		msgq(sp, M_ERR, "065|The modeline(s) option may never be set");
	return (0);
}

DECL(f_msgcat)
{
	(void)msg_open(sp, str);
	return (opt_dup(sp, O_MSGCAT, str));
}

DECL(f_number)
{
	if (turnoff)
		O_CLR(sp, O_NUMBER);
	else
		O_SET(sp, O_NUMBER);

	F_SET(sp, S_SCR_REFORMAT);
	return (0);
}

DECL(f_octal)
{
	if (turnoff)
		O_CLR(sp, O_OCTAL);
	else
		O_SET(sp, O_OCTAL);

	key_init(sp);
	F_SET(sp, S_SCR_REFORMAT);
	return (0);
}

DECL(f_paragraph)
{
	if (strlen(str) & 1) {
		msgq(sp, M_ERR,
		    "066|The paragraph option must be in two character groups");
		return (1);
	}
	return (opt_dup(sp, O_PARAGRAPHS, str));
}

DECL(f_readonly)
{
	if (turnoff) {
		O_CLR(sp, O_READONLY);
		if (sp->frp != NULL)
			F_CLR(sp->frp, FR_RDONLY);
	} else {
		O_SET(sp, O_READONLY);
		if (sp->frp != NULL)
			F_SET(sp->frp, FR_RDONLY);
	}
	return (0);
}

DECL(f_section)
{
	if (strlen(str) & 1) {
		msgq(sp, M_ERR,
		    "067|The section option must be in two character groups");
		return (1);
	}
	return (opt_dup(sp, O_SECTIONS, str));
}

DECL(f_shiftwidth)
{
	if (val == 0) {
		msgq(sp, M_ERR, "068|The shiftwidth may not be set to 0");
		return (1);
	}
	O_VAL(sp, O_SHIFTWIDTH) = val;
	return (0);
}

/*
 * f_sourceany --
 *	Historic vi, on startup, source'd $HOME/.exrc and ./.exrc, if they
 *	were owned by the user.  The sourceany option was an undocumented
 *	feature of historic vi which permitted the startup source'ing of
 *	.exrc files the user didn't own.  This is an obvious security problem,
 *	and we ignore the option.
 */
DECL(f_sourceany)
{
	if (!turnoff)
		msgq(sp, M_ERR, "069|The sourceany option may never be set");
	return (0);
}

DECL(f_tabstop)
{
	if (val == 0) {
		msgq(sp, M_ERR, "070|Tab stops may not be set to 0");
		return (1);
	}
	O_VAL(sp, O_TABSTOP) = val;

	F_SET(sp, S_SCR_REFORMAT);
	return (0);
}

DECL(f_tags)
{
	return (opt_dup(sp, O_TAGS, str));
}

DECL(f_term)
{
	char buf[256];

	if (opt_dup(sp, O_TERM, str))
		return (1);

	/* Set the terminal value in the environment for curses. */
	(void)snprintf(buf, sizeof(buf), "TERM=%s", str);
	if (opt_putenv(buf))
		return (1);

	/* Reset the screen size. */
	if (term_window(sp, 0))
		return (1);
	F_SET(sp, S_SCR_RESIZE);

	return (0);
}

DECL(f_ttywerase)
{
	if (turnoff)
		O_CLR(sp, O_TTYWERASE);
	else {
		O_SET(sp, O_TTYWERASE);
		O_CLR(sp, O_ALTWERASE);
	}
	return (0);
}

DECL(f_w300)
{
	/* Historical behavior for w300 was < 1200. */
	if (baud_from_bval(sp) >= 1200)
		return (0);

	if (CALL(f_window))
		return (1);

	if (val > O_VAL(sp, O_LINES) - 1)
		val = O_VAL(sp, O_LINES) - 1;
	O_VAL(sp, O_W300) = val;
	return (0);
}

DECL(f_w1200)
{
	u_long v;

	/* Historical behavior for w1200 was == 1200. */
	v = baud_from_bval(sp);
	if (v < 1200 || v > 4800)
		return (0);

	if (CALL(f_window))
		return (1);

	if (val > O_VAL(sp, O_LINES) - 1)
		val = O_VAL(sp, O_LINES) - 1;
	O_VAL(sp, O_W1200) = val;
	return (0);
}

DECL(f_w9600)
{
	speed_t v;

	/* Historical behavior for w9600 was > 1200. */
	v = baud_from_bval(sp);
	if (v <= 4800)
		return (0);

	if (CALL(f_window))
		return (1);

	if (val > O_VAL(sp, O_LINES) - 1)
		val = O_VAL(sp, O_LINES) - 1;
	O_VAL(sp, O_W9600) = val;
	return (0);
}

DECL(f_window)
{
	if (val < MINIMUM_SCREEN_ROWS) {
		msgq(sp, M_ERR,
		    "072|Window too small, less than %d", MINIMUM_SCREEN_ROWS);
		return (1);
	}
	if (val > O_VAL(sp, O_LINES) - 1)
		val = O_VAL(sp, O_LINES) - 1;
	O_VAL(sp, O_WINDOW) = val;

	return (0);
}

/*
 * opt_dup --
 *	Copy a string value for user display.
 */
static int
opt_dup(sp, opt, str)
	SCR *sp;
	int opt;
	char *str;
{
	char *p;

	/* Copy for user display. */
	if ((p = strdup(str)) == NULL) {
		msgq(sp, M_SYSERR, NULL);
		return (1);
	}
	if (O_STR(sp, opt) != NULL)
		free(O_STR(sp, opt));
	O_STR(sp, opt) = p;
	return (0);
}

/*
 * opt_putenv --
 *	Put a value into the environment.  We use putenv(3) because it's
 *	more portable.  The following hack is because some moron decided
 *	to keep a reference to the memory passed to putenv(3), instead of
 *	having it allocate its own.  Someone clearly needs to get promoted
 *	into management.
 */
static int
opt_putenv(s)
	char *s;
{
	char *t;

	/*
	 * XXX
	 * Memory leak.
	 */
	if ((t = strdup(s)) == NULL)
		return (1);
	return (putenv(t));
}

/*
 * baud_from_bval --
 *	Return the baud rate using the standard defines.
 */
u_long
baud_from_bval(sp)
	SCR *sp;
{
	if (!F_ISSET(sp->gp, G_TERMIOS_SET))
		return (9600);

	/*
	 * XXX
	 * There's no portable way to get a "baud rate" -- cfgetospeed(3)
	 * returns the value associated with some #define, which we may
	 * never have heard of, or which may be a purely local speed.  Vi
	 * only cares if it's SLOW (w300), slow (w1200) or fast (w9600).
	 * Try and detect the slow ones, and default to fast.
	 */
	switch (cfgetospeed(&sp->gp->original_termios)) {
	case B50:
	case B75:
	case B110:
	case B134:
	case B150:
	case B200:
	case B300:
	case B600:
		return (600);
	case B1200:
		return (1200);
	}
	return (9600);
}
