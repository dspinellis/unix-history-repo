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
static char sccsid[] = "@(#)svi_ex.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <curses.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"
#include "excmd.h"
#include "svi_screen.h"

static int	svi_ex_scroll __P((SCR *, int, int, int *));
static int	svi_ex_done __P((SCR *, EXF *, MARK *));

/*
 * svi_ex_cmd --
 *	Execute an ex command.
 */
int
svi_ex_cmd(sp, ep, exp, rp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *exp;
	MARK *rp;
{
	int rval;

	sp->exlcontinue = sp->exlinecount = sp->extotalcount = 0;

	(void)svi_busy_cursor(sp, NULL);
	rval = exp->cmd->fn(sp, ep, exp);

	msg_rpt(sp, sp->stdfp);
	(void)fflush(sp->stdfp);

	/* If only one line, don't wait. */
	if (sp->extotalcount >= 1)
		if (sp->extotalcount == 1)
			F_SET(sp, S_UPDATE_MODE);
		else
			(void)svi_ex_scroll(sp, 1, 0, NULL);

	return (svi_ex_done(sp, ep, rp) || rval);
}

/*
 * svi_ex_run --
 *	Execute strings of ex commands.
 */
int
svi_ex_run(sp, ep, rp)
	SCR *sp;
	EXF *ep;
	MARK *rp;
{
	TEXT *tp;
	int key;

	sp->exlcontinue = sp->exlinecount = sp->extotalcount = 0;
	for (;;) {
		/* Get an ex command. */
		if (svi_get(sp, ep, &sp->bhdr, ':', TXT_BS | TXT_PROMPT))
			break;

		tp = sp->bhdr.next;
		if (tp->len == 1)
			break;

		(void)svi_busy_cursor(sp, NULL);
		(void)ex_cstring(sp, ep, tp->lb, tp->len);
		(void)fflush(sp->stdfp);

		/*
		 * The file or screen may have changed, in which case,
		 * the main editor loop takes care of it.
		 */
		if (F_ISSET(sp, S_MAJOR_CHANGE))
			break;
		
		/* If only one line, don't wait. */
		if (sp->extotalcount <= 1) {
			if (sp->extotalcount == 1)
				F_SET(sp, S_UPDATE_MODE);
			break;
		}

		/* The user may continue in ex mode by entering a ':'. */
		(void)svi_ex_scroll(sp, 1, 1, &key);
		if (key != ':')
                        break;

		++sp->extotalcount;
		++sp->exlinecount;
	}
	(void)svi_ex_done(sp, ep, rp);
	return (0);
}

/*
 * svi_ex_done --
 *	Cleanup from dipping into ex.
 */
static int
svi_ex_done(sp, ep, rp)
	SCR *sp;
	EXF *ep;
	MARK *rp;
{
	recno_t lno;
	size_t len, line_off;

	/*
	 * The file or screen may have changed, in which case,
	 * the main editor loop takes care of it.
	 */
	if (F_ISSET(sp, S_MAJOR_CHANGE))
		return (0);

	/*
	 * Otherwise, the only cursor modifications will be real, however, the
	 * underlying line may have changed; don't trust anything.  This code
	 * has been a remarkably fertile place for bugs.
	 *
	 * Repaint the entire screen if at least half the screen is trashed.
	 * Else, repaint only over the overwritten lines.  The "-2" comes
	 * from one for the mode line and one for the fact that it's an offset.
	 *
	 * Don't trust ANYTHING.
	 */
	if (sp->extotalcount > 1)
		if (sp->extotalcount >= HALFSCREEN(sp))
			F_SET(sp, S_REDRAW);
		else
			for (line_off = sp->rows - 2;
			    sp->extotalcount--; --line_off)
				if (svi_line(sp, ep,
				    HMAP + line_off, NULL, 0, NULL, NULL))
					return (1);

	/*
	 * Do a reality check on a cursor value, and make sure it's okay.
	 * If necessary, change it, but keep it as close as possible to
	 * the claimed value.  The main reason is to make sure that we
	 * haven't lost because ex doesn't care about the column and it's
	 * disappeared.
	 */
	lno = sp->lno;
	if (file_gline(sp, ep, lno, &len) == NULL) {
		if (file_lline(sp, ep, &lno))
			return (1);
		if (lno == 0) {
			sp->lno = 1;
			sp->cno = 0;
		} else {
			GETLINE_ERR(sp, sp->lno);
			if (file_gline(sp, ep, lno, &len) == NULL) {
				sp->lno = 1;
				sp->cno = 0;
			} else
				sp->cno = sp->s_relative(sp, ep, sp->lno);
		}
	} else if (sp->cno >= len)
		sp->cno = len ? len - 1 : 0;

	rp->lno = sp->lno;
	rp->cno = sp->cno;
	return (0);
}

/*
 * svi_ex_write --
 *	Write out the ex messages.
 */
int
svi_ex_write(cookie, line, llen)
	void *cookie;
	const char *line;
	int llen;
{
	SCR *sp;
	size_t new_lcontinue;
	int len, rlen;
	const char *p;

	new_lcontinue = 0;		/* In case of a write of 0. */
	p = line;

	rlen = llen;
	for (sp = cookie; llen;) {
		/* Get the next line. */
		if ((p = memchr(line, '\n', llen)) == NULL)
			len = llen;
		else
			len = p - line;

		/*
		 * The max is sp->cols characters, and we may
		 * have already written part of the line.
		 */
		if (len + sp->exlcontinue > sp->cols)
			len = sp->cols - sp->exlcontinue;

		/*
		 * If the first line output, do nothing.
		 * If the second line output, move the screen up and draw the
		 * divider line.
		 * Else, if it's a continuation line, move to the continuation
		 * point, else, move the screen up.
		 */
		if (sp->exlcontinue == 0) {
			if (sp->extotalcount == 1) {
				MOVE(sp, INFOLINE(sp) - 1, 0);
				clrtoeol();
				if (svi_divider(sp))
					return (-1);
				++sp->extotalcount;
				++sp->exlinecount;
			}
			if (sp->extotalcount != 0 &&
			    svi_ex_scroll(sp, 0, 0, NULL))
				return (-1);
			MOVE(sp, INFOLINE(sp), 0);
			++sp->extotalcount;
			++sp->exlinecount;
		} else
			MOVE(sp, INFOLINE(sp), sp->exlcontinue);

		/* Display the line. */
		if (len)
			ADDNSTR(line, len);

		/* Clear to EOL. */
		if (len + sp->exlcontinue < sp->cols)
			clrtoeol();

		/* Set up exlcontinue. */
		new_lcontinue = len + sp->exlcontinue;
		sp->exlcontinue = 0;

		/* Reset for the next line. */
		line += len;
		llen -= len;
		if (p != NULL) {
			++line;
			--llen;
		}
	}
	/* Refresh the screen, even if it's a partial. */
	refresh();

	/* Set up next continuation line. */
	if (p == NULL)
		sp->exlcontinue = new_lcontinue;
	return (rlen);
}

/*
 * svi_ex_scroll --
 *	Scroll the screen for ex output.
 */
static int
svi_ex_scroll(sp, mustwait, colon_ok, chp)
	SCR *sp;
	int mustwait, colon_ok, *chp;
{
	int ch;

	/*
	 * Scroll the screen.  Instead of scrolling the entire screen, delete
	 * the line above the first line output so preserve the maximum amount
	 * of the screen.
	 */
	if (sp->extotalcount >= sp->rows) {
		MOVE(sp, 0, 0);
	} else
		MOVE(sp, INFOLINE(sp) - sp->extotalcount, 0);

	deleteln();
	if (sp->child != NULL) {
		MOVE(sp, INFOLINE(sp), 0);
		insertln();
	}

	/* If just displayed a full screen, wait. */
	if (mustwait || sp->exlinecount == sp->rows) {
		MOVE(sp, INFOLINE(sp), 0);
		ADDNSTR(CONTMSG, (int)sizeof(CONTMSG) - 1);
		clrtoeol();
		refresh();
		while (sp->special[ch = term_key(sp, 0)] != K_CR &&
		    !isspace(ch) && (!colon_ok || ch != ':'))
			svi_bell(sp);
		if (chp != NULL)
			*chp = ch;
		sp->exlinecount = 0;
	}
	return (0);
}
