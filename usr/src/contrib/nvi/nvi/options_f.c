/*-
 * Copyright (c) 1993
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
static char sccsid[] = "@(#)options_f.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "tag.h"

#define	DECL(f)	int							\
	f(sp, op, str, val)						\
		SCR *sp;						\
		OPTION *op;						\
		char *str;						\
		u_long val;
#define	turnoff	val

static int ps_list __P((SCR *));

DECL(f_columns)
{
	char buf[25];

	/* Validate the number. */
	if (val < MINIMUM_SCREEN_COLS) {
		msgq(sp, M_ERR, "Screen columns too small, less than %d.",
		    MINIMUM_SCREEN_COLS);
		return (1);
	}
	if (val < O_VAL(sp, O_SHIFTWIDTH)) {
		msgq(sp, M_ERR,
		    "Screen columns too small, less than shiftwidth.");
		return (1);
	}
	if (val < O_VAL(sp, O_SIDESCROLL)) {
		msgq(sp, M_ERR,
		    "Screen columns too small, less than sidescroll.");
		return (1);
	}
	if (val < O_VAL(sp, O_TABSTOP)) {
		msgq(sp, M_ERR,
		    "Screen columns too small, less than tabstop.");
		return (1);
	}
	if (val < O_VAL(sp, O_WRAPMARGIN)) {
		msgq(sp, M_ERR,
		    "Screen columns too small, less than wrapmargin.");
		return (1);
	}
#ifdef XXX_NOT_RIGHT
	/*
	 * This has to be checked by reaching down into the screen code.
	 */
	if (val < O_NUMBER_LENGTH) {
		msgq(sp, M_ERR,
		    "Screen columns too small, less than number option.");
		return (1);
	}
#endif
	/* This is expensive, don't do it unless it's necessary. */
	if (O_VAL(sp, O_COLUMNS) == val)
		return (0);

	/* Set the value. */
	O_VAL(sp, O_COLUMNS) =  val;

	/* Set the columns value in the environment for curses. */
	(void)snprintf(buf, sizeof(buf), "COLUMNS=%lu", val);
	(void)putenv(buf);

	F_SET(sp, S_RESIZE | S_REFORMAT | S_REDRAW);
	return (0);
}

DECL(f_flash)
{
	size_t len;
	char *s, *t, b1[2048], b2[2048];
	
	if (turnoff) {
		O_CLR(sp, O_FLASH);
		return (0);
	}

	/* Get the termcap information. */
	s = O_STR(sp, O_TERM);
	if (tgetent(b1, s) != 1) {
		msgq(sp, M_ERR, "No termcap entry for %s", s);
		return (1);
	}

	/* Get the visual bell string. */
	t = b2;
	if (tgetstr("vb", &t) == NULL) {
		msgq(sp, M_VINFO, "No visual bell for %s terminal type", s);
		O_CLR(sp, O_FLASH);
		return (0);
	}

	len = t - b2;
	if ((s = malloc(len)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}

	memmove(s, b2, len);

	if (sp->VB != NULL)
		free(sp->VB);
	sp->VB = t;

	O_SET(sp, O_FLASH);
	return (0);
}

DECL(f_keytime)
{
#define	MAXKEYTIME	20
	if (val > MAXKEYTIME) {
		msgq(sp, M_ERR,
		    "Keytime too large, more than %d.", MAXKEYTIME);
		return (1);
	}
	O_VAL(sp, O_KEYTIME) = val;
	return (0);
}

DECL(f_leftright)
{
	if (turnoff)
		O_CLR(sp, O_LEFTRIGHT);
	else
		O_SET(sp, O_LEFTRIGHT);
	F_SET(sp, S_REFORMAT | S_REDRAW);
	return (0);
}

DECL(f_lines)
{
	char buf[25];

	/* Validate the number. */
	if (val < MINIMUM_SCREEN_ROWS) {
		msgq(sp, M_ERR, "Screen lines too small, less than %d.",
		    MINIMUM_SCREEN_ROWS);
		return (1);
	}

	/* This is expensive, don't do it unless it's necessary. */
	if (O_VAL(sp, O_LINES) == val)
		return (0);

	/* Set the value. */
	O_VAL(sp, O_LINES) =  val;

	/* Set the columns value in the environment for curses. */
	(void)snprintf(buf, sizeof(buf), "ROWS=%lu", val);
	(void)putenv(buf);

	F_SET(sp, S_RESIZE | S_REFORMAT | S_REDRAW);
	return (0);
}

DECL(f_list)
{
	if (turnoff)
		O_CLR(sp, O_LIST);
	else
		O_SET(sp, O_LIST);

	F_SET(sp, S_REFORMAT | S_REDRAW);
	return (0);
}

DECL(f_mesg)
{
	struct stat sb;
	char *tty;

	/* Find the tty. */
	if ((tty = ttyname(STDERR_FILENO)) == NULL) {
		msgq(sp, M_ERR, "ttyname: %s", strerror(errno));
		return (1);
	}

	/* Save the tty mode for later; only save it once. */
	if (!F_ISSET(sp->gp, G_SETMODE)) {
		F_SET(sp->gp, G_SETMODE);
		if (stat(tty, &sb) < 0) {
			msgq(sp, M_ERR, "%s: %s", strerror(errno));
			return (1);
		}
		sp->gp->origmode = sb.st_mode;
	}

	if (turnoff) {
		if (chmod(tty, sb.st_mode & ~S_IWGRP) < 0) {
			msgq(sp, M_ERR, "%s: %s", strerror(errno));
			return (1);
		}
		O_CLR(sp, O_MESG);
	} else {
		if (chmod(tty, sb.st_mode | S_IWGRP) < 0) {
			msgq(sp, M_ERR, "%s: %s", strerror(errno));
			return (1);
		}
		O_SET(sp, O_MESG);
	}
	return (0);
}

DECL(f_modelines)
{
	if (!turnoff)
		msgq(sp, M_ERR, "The modelines option may never be set");
	return (0);
}

DECL(f_number)
{
	if (turnoff)
		O_CLR(sp, O_NUMBER);
	else
		O_SET(sp, O_NUMBER);

	F_SET(sp, S_REFORMAT | S_REDRAW);
	return (0);
}

DECL(f_paragraph)
{
	if (strlen(str) & 1) {
		msgq(sp, M_ERR,
		    "Paragraph options must be in sets of two characters.");
		return (1);
	}

	if (F_ISSET(&sp->opts[O_PARAGRAPHS], OPT_ALLOCATED))
		free(O_STR(sp, O_PARAGRAPHS));
	if ((O_STR(sp, O_PARAGRAPHS) = strdup(str)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	F_SET(&sp->opts[O_PARAGRAPHS], OPT_ALLOCATED | OPT_SET);
	return (ps_list(sp));
}

DECL(f_readonly)
{
	if (turnoff) {
		O_CLR(sp, O_READONLY);
		F_CLR(sp->ep, F_RDONLY);
	} else {
		O_SET(sp, O_READONLY);
		F_SET(sp->ep, F_RDONLY);
	}
	return (0);
}

DECL(f_ruler)
{
	if (turnoff)
		O_CLR(sp, O_RULER);
	else
		O_SET(sp, O_RULER);
	return (0);
}

DECL(f_section)
{
	if (strlen(str) & 1) {
		msgq(sp, M_ERR,
		    "Section options must be in sets of two characters.");
		return (1);
	}

	if (F_ISSET(&sp->opts[O_SECTIONS], OPT_ALLOCATED))
		free(O_STR(sp, O_SECTIONS));
	if ((O_STR(sp, O_SECTIONS) = strdup(str)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	F_SET(&sp->opts[O_SECTIONS], OPT_ALLOCATED | OPT_SET);
	return (ps_list(sp));
}

DECL(f_shiftwidth)
{
	if (val == 0) {
		msgq(sp, M_ERR, "The shiftwidth can't be set to 0.");
		return (1);
	}
	if (val > O_VAL(sp, O_COLUMNS)) {
		msgq(sp, M_ERR,
		    "Shiftwidth can't be larger than screen size.");
		return (1);
	}
	O_VAL(sp, O_SHIFTWIDTH) = val;
	return (0);
}

DECL(f_sidescroll)
{
	if (val > O_VAL(sp, O_COLUMNS)) {
		msgq(sp, M_ERR,
		    "Sidescroll can't be larger than screen size.");
		return (1);
	}
	O_VAL(sp, O_SIDESCROLL) = val;
	return (0);
}

DECL(f_tabstop)
{
	if (val == 0) {
		msgq(sp, M_ERR, "Tab stops can't be set to 0.");
		return (1);
	}
#define	MAXTABSTOP	20
	if (val > MAXTABSTOP) {
		msgq(sp, M_ERR,
		    "Tab stops can't be larger than %d.", MAXTABSTOP);
		return (1);
	}
	if (val > O_VAL(sp, O_COLUMNS)) {
		msgq(sp, M_ERR,
		    "Tab stops can't be larger than screen size.",
		    MAXTABSTOP);
		return (1);
	}
	O_VAL(sp, O_TABSTOP) = val;

	F_SET(sp, S_REFORMAT | S_REDRAW);
	return (0);
}

/*
 * f_tags --
 *	Build an array of pathnames for the tags routines.  It's not
 *	a fast build as we walk the string twice, but it's unclear we
 *	care.
 */
DECL(f_tags)
{
	size_t len;
	int cnt;
	char *p, *t;

						/* Free up previous array. */
	if (F_ISSET(&sp->opts[O_TAGS], OPT_ALLOCATED) && sp->tfhead != NULL) {
		for (cnt = 0; sp->tfhead[cnt] != NULL; ++cnt)
			free(sp->tfhead[cnt]->fname);
		free(sp->tfhead);
	}
						/* Copy for user display. */
	if (F_ISSET(&sp->opts[O_TAGS], OPT_ALLOCATED))
		free(O_STR(sp, O_TAGS));
	if ((O_STR(sp, O_TAGS) = strdup(str)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	F_SET(&sp->opts[O_TAGS], OPT_ALLOCATED);

	for (p = t = str, cnt = 0;; ++p) {	/* Count new entries. */
		if (*p == '\0' || isspace(*p)) {
			if ((len = p - t) > 1)
				++cnt;
			t = p + 1;
		}
		if (*p == '\0')
			break;
	}
						/* Allocate new array. */
	if ((sp->tfhead = malloc((cnt + 1) * sizeof(TAGF))) == NULL)
		goto mem2;
	sp->tfhead[cnt] = NULL;
	for (p = t = str, cnt = 0;; ++p) {	/* Fill in new array. */
		if (*p == '\0' || isspace(*p)) {
			if ((len = p - t) > 1) {
				if ((sp->tfhead[cnt] =
				    malloc(sizeof(TAGF))) == NULL)
					goto mem1;
				if ((sp->tfhead[cnt]->fname =
				    malloc(len + 1)) == NULL) {
mem1:					sp->tfhead[cnt] = NULL;
mem2:					msgq(sp, M_ERR,
					    "Error: %s", strerror(errno));
					return (1);
				}
				memmove(sp->tfhead[cnt]->fname, t, len);
				sp->tfhead[cnt]->fname[len] = '\0';
				sp->tfhead[cnt]->flags = 0;
				++cnt;
			}
			t = p + 1;
		}
		if (*p == '\0')
			 break;
	}
	F_SET(&sp->opts[O_TAGS], OPT_SET);
	return (0);
}

DECL(f_term)
{
	if (F_ISSET(&sp->opts[O_TERM], OPT_ALLOCATED))
		free(O_STR(sp, O_TERM));
	if ((O_STR(sp, O_TERM) = strdup(str)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	F_SET(&sp->opts[O_TERM], OPT_ALLOCATED | OPT_SET);

	/* Change the flash value if it's set. */
	if (O_ISSET(sp, O_FLASH) && f_flash(sp, op, NULL, 0))
		msgq(sp, M_ERR,
		    "Term value %s; unable to set flash option.", str);

	(void)set_window_size(sp, 0);
	return (0);
}

DECL(f_wrapmargin)
{
	if (val > O_VAL(sp, O_COLUMNS)) {
		msgq(sp, M_ERR,
		    "Wrapmargin value can't be larger than screen size.");
		return (1);
	}
	O_VAL(sp, O_WRAPMARGIN) = val;
	return (0);
}

static int
ps_list(sp)
	SCR *sp;
{
	size_t p_len, s_len;
	char *p_p, *s_p;
	char *p;

	/*
	 * The vi paragraph command searches for either a paragraph or
	 * section option macro.
	 */
	p_len = (p_p = O_STR(sp, O_PARAGRAPHS)) == NULL ? 0 : strlen(p_p);
	s_len = (s_p = O_STR(sp, O_SECTIONS)) == NULL ? 0 : strlen(s_p);

	if (p_len == 0 && s_len == 0)
		return (0);

	if ((p = malloc(p_len + s_len + 1)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}

	if (sp->paragraph != NULL)
		FREE(sp->paragraph, strlen(sp->paragraph) + 1);

	if (p_p != NULL)
		memmove(p, p_p, p_len);
	if (s_p != NULL)
		memmove(p + p_len, s_p, s_len + 1);
	sp->paragraph = p;
	return (0);
}
