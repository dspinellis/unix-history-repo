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
static char sccsid[] = "@(#)svi_ex.c	9.8 (Berkeley) 12/2/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
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
#include <curses.h>
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "../vi/vcmd.h"
#include "excmd.h"
#include "svi_screen.h"
#include "../sex/sex_screen.h"

static int	svi_ex_divider __P((SCR *));
static int	svi_ex_done __P((SCR *, MARK *, int));
static int	svi_ex_inv __P((SCR *));

#define	SCROLL_QUIT	0x01		/* User can enter 'q' to interrupt. */
#define	SCROLL_WAIT	0x02		/* User must enter continuation char. */
static int	svi_ex_scroll __P((SCR *, CH *, u_int));

#define	MSGS_WAITING(sp)						\
	((sp)->msgq.lh_first != NULL &&					\
	    !F_ISSET((sp)->msgq.lh_first, M_EMPTY))

/*
 * svi_ex_cmd --
 *	Execute an ex command.
 */
int
svi_ex_cmd(sp, exp, rp)
	SCR *sp;
	EXCMDARG *exp;
	MARK *rp;
{
	SVI_PRIVATE *svp;
	int rval;

	svp = SVP(sp);
	svp->exlcontinue = svp->exlinecount = svp->extotalcount = 0;

	(void)svi_busy(sp, NULL);
	rval = exp->cmd->fn(sp, exp);

	(void)msg_rpt(sp, 0);
	(void)ex_fflush(EXCOOKIE);

	/*
	 * If displayed anything, figure out if we have to wait.  If the
	 * screen wasn't trashed, only one line output and there are no
	 * waiting messages, don't wait, but don't overwrite it with mode
	 * information either.
	 */
	if (svp->extotalcount > 0)
		if (!F_ISSET(sp, S_SCR_REFRESH) &&
		    svp->extotalcount == 1 && !MSGS_WAITING(sp)) {
			F_SET(sp, S_SCR_UMODE);
			if (sp->q.cqe_next != (void *)&sp->gp->dq)
				(void)svi_ex_inv(sp);
		} else
			(void)svi_ex_scroll(sp, NULL, SCROLL_WAIT);
	return (svi_ex_done(sp, rp, 0) || rval);
}

/*
 * svi_ex_run --
 *	Execute strings of ex commands.
 */
int
svi_ex_run(sp, rp)
	SCR *sp;
	MARK *rp;
{
	enum input (*get) __P((SCR *, TEXTH *, ARG_CHAR_T, u_int));
	struct termios t;
	CH ikey;
	FILE *sfp;
	SVI_PRIVATE *svp;
	TEXT *tp;
	int flags, in_exmode, rval;

	svp = SVP(sp);
	svp->exlcontinue = svp->exlinecount = svp->extotalcount = 0;

	/*
	 * There's some tricky stuff going on here to handle when a user has
	 * mapped a key to multiple ex commands.  Historic practice was that
	 * vi ran without any special actions, as if the user were entering
	 * the characters, until ex trashed the screen, e.g. something like a
	 * '!' command.  At that point, we no longer know what the screen
	 * looks like, so we can't afford to overwrite anything.  The solution
	 * is to go into real ex mode until we get to the end of the command
	 * strings.
	 */
	get = svi_get;
	flags = TXT_BS | TXT_PROMPT;
	for (in_exmode = rval = 0;;) {
		/* Get the next command. */
		if (get(sp, sp->tiqp, ':', flags) != INP_OK) {
			rval = 1;
			break;
		}
		if (INTERRUPTED(sp))
			break;

		/*
		 * Len is 0 if the user backspaced over the prompt,
		 * 1 if only a CR was entered.
		 */
		tp = sp->tiqp->cqh_first;
		if (tp->len == 0)
			break;

		if (!in_exmode)
			(void)svi_busy(sp, NULL);

		/*
		 * Ignore return, presumably an error message was displayed.
		 * Don't specify any of the EX_PARSE_* flags, we get the right
		 * behavior for free because we're in vi mode.
		 */
		(void)ex_icmd(sp, tp->lb, tp->len, 0);
		(void)ex_fflush(EXCOOKIE);

		/*
		 * The file or screen may have changed, in which case, the
		 * main editor loop takes care of it.
		 */
		if (F_ISSET(sp, S_MAJOR_CHANGE))
			break;

		/*
		 * If continue not required, and one or no lines, and there
		 * are no waiting messages, don't wait, but don't overwrite
		 * it with mode information either.
		 */
		if (!in_exmode && !F_ISSET(sp, S_CONTINUE) &&
		    (svp->extotalcount == 0 ||
		    svp->extotalcount == 1 && !MSGS_WAITING(sp))) {
			if (svp->extotalcount == 1) {
				F_SET(sp, S_SCR_UMODE);
				if (sp->q.cqe_next != (void *)&sp->gp->dq)
					svi_ex_inv(sp);
			}
			break;
		}

		if (INTERRUPTED(sp))
			break;

		/*
		 * If the screen is trashed, or there are messages waiting,
		 * go into ex mode.  When we first make the transition, put
		 * out a <newline> to separate the current output from the
		 * future output.  (Future output will be separated already
		 * because we'll be running in ex mode.)
		 */
		if (!in_exmode &&
		    (F_ISSET(sp, S_SCR_REFRESH) || MSGS_WAITING(sp))) {
			if (F_ISSET(sp->gp, G_STDIN_TTY))
				SEX_RAW(&t);
			get = sex_get;
			flags = TXT_CR | TXT_NLECHO | TXT_PROMPT;
			sfp = sp->stdfp;
			sp->stdfp = stdout;
			in_exmode = 1;

			if (MSGS_WAITING(sp))
				(void)write(STDOUT_FILENO, "\n", 1);
		}
		if (MSGS_WAITING(sp))
			(void)sex_refresh(sp);

		/*
		 * Get a continue character; users may continue in ex mode by
		 * entering a ':'.
		 *
		 * !!!
		 * Historic practice is that any key can be used to continue.
		 * Nvi used to require that the user enter a <carriage-return>
		 * or <newline>, but this broke historic users.
		 */
		if (in_exmode) {
			(void)write(STDOUT_FILENO,
			    STR_CMSG, sizeof(STR_CMSG) - 1);
			if (term_key(sp, &ikey, 0) != INP_OK) {
				rval = 1;
				goto ret;
			}
		} else
			(void)svi_ex_scroll(sp, &ikey, SCROLL_WAIT);
		if (INTERRUPTED(sp) || ikey.ch != ':')
			break;

		if (in_exmode)
			(void)write(STDOUT_FILENO, "\n", 1);
		else {
			++svp->extotalcount;
			++svp->exlinecount;
		}
	}

ret:	if (in_exmode) {
		/* Reset the terminal state. */
		if (F_ISSET(sp->gp, G_STDIN_TTY) && SEX_NORAW(&t))
			rval = 1;
		sp->stdfp = sfp;
		F_SET(sp, S_SCR_REDRAW | S_SCR_REFRESH);
	}
	if (svi_ex_done(sp, rp, in_exmode))
		rval = 1;

	F_CLR(sp, S_CONTINUE);
	return (rval);
}

/*
 * svi_msgflush --
 *	Flush any accumulated messages.
 */
int
svi_msgflush(sp)
	SCR *sp;
{
	enum {INVERSE, NORMAL} inverse;
	SVI_PRIVATE *svp;
	MSG *mp;
	size_t len;
	int first, global, rval;
	char *e, *s, *t;

	svp = SVP(sp);
	svp->exlcontinue = svp->exlinecount = svp->extotalcount = 0;

	first = global = 1;
	inverse = NORMAL;
	mp = sp->gp->msgq.lh_first;
mloop:	for (; mp != NULL && !F_ISSET(mp, M_EMPTY); mp = mp->q.le_next) {
		/*
		 * All messages start at the beginning of a line.  If any
		 * message won't fit on a line, try to break it at a <blank>
		 * boundary.  If a second or subsequent message will fit on
		 * the a line, write a separator, and output it.  Otherwise,
		 * put out a newline.
		 */
		if (!first)
			if (mp->len + svp->exlcontinue + 3 >= sp->cols) {
				if (inverse == INVERSE)
					F_SET(svp, SVI_SCR_IVIDEO);
				(void)svi_ex_write(sp, ".\n", 2);
				F_CLR(svp, SVI_SCR_IVIDEO);
			} else  {
				if (inverse == INVERSE)
					F_SET(svp, SVI_SCR_IVIDEO);
				(void)svi_ex_write(sp, ";", 1);
				F_CLR(svp, SVI_SCR_IVIDEO);
				(void)svi_ex_write(sp, "  ", 2);
			}
		else
			first = 0;

		inverse = F_ISSET(mp, M_INV_VIDEO) ? INVERSE : NORMAL;
		if (inverse == INVERSE)
			F_SET(svp, SVI_SCR_IVIDEO);
		for (s = mp->mbuf,
		    len = mp->len; len != 0; s = t) {
			for (e = t = s + len; e - s >= sp->cols;) {
				for (; --e > s && !isblank(*e););
				if (e == s) {
					e = t;
					goto noblank;
				} else {
					for (t = e + 1;
					    --e > s && isblank(*e););
					if (e == s)
						goto ws_skip;
					++e;
				}
			}
noblank:		if ((e - s) > 1 && s[(e - s) - 1] == '.')
				--e;
			(void)svi_ex_write(sp, s, e - s);
ws_skip:		len -= t - s;
			if (len != 0)
				(void)svi_ex_write(sp, "\n", 1);
		}
		F_CLR(svp, SVI_SCR_IVIDEO);

		F_SET(mp, M_EMPTY);
	}

	/* Two queues, global and screen local.  Flush both. */
	if (global) {
		global = 0;
		mp = sp->msgq.lh_first;
		goto mloop;
	}

	/*
	 * None of the messages end with periods, we do it in the message
	 * flush routine, which makes it possible to join messages.
	 */
	if (inverse == INVERSE)
		F_SET(svp, SVI_SCR_IVIDEO);
	(void)svi_ex_write(sp, ".", 1);
	F_CLR(svp, SVI_SCR_IVIDEO);

	/*
	 * Figure out if we have to wait.  Don't wait for only one line,
	 * but don't overwrite it with mode information either.
	 */
	if (svp->extotalcount == 1) {
		F_SET(sp, S_SCR_UMODE);
		if (sp->q.cqe_next != (void *)&sp->gp->dq)
			svi_ex_inv(sp);
		return (0);
	}

	rval = svi_ex_scroll(sp, NULL, SCROLL_WAIT);
	if (svi_ex_done(sp, NULL, 0))
		rval = 1;
	MOVE(sp, INFOLINE(sp), 0);
	clrtoeol();
	return (rval);
}

/*
 * svi_ex_done --
 *	Cleanup from dipping into ex.
 */
static int
svi_ex_done(sp, rp, nopaint)
	SCR *sp;
	MARK *rp;
	int nopaint;
{
	SMAP *smp;
	SVI_PRIVATE *svp;
	recno_t lno;
	size_t cnt, len;

	/*
	 * The file or screen may have changed, in which case, the main
	 * editor loop takes care of it.
	 */
	if (F_ISSET(sp, S_MAJOR_CHANGE))
		return (0);

	/*
	 * Unless explicitly told not to repaint, repaint the entire screen
	 * if at least half the screen is trashed.  Else, repaint only over
	 * the overwritten lines.  The "-2" comes from one for the mode line
	 * and one for the fact that it's an offset.  Note the check for
	 * small screens.
	 *
	 * Don't trust ANYTHING.
	 */
	if (!nopaint) {
		svp = SVP(sp);
		if (svp->extotalcount >= HALFTEXT(sp))
			F_SET(sp, S_SCR_REDRAW);
		else
			for (cnt = sp->rows - 2; svp->extotalcount--; --cnt)
				if (cnt > sp->t_rows) {
					MOVE(sp, cnt, 0);
					clrtoeol();
				} else {
					smp = HMAP + cnt;
					SMAP_FLUSH(smp);
					if (svi_line(sp, smp, NULL, NULL))
						return (1);
				}
	}

	/* Ignore the cursor if the caller doesn't care. */
	if (rp == NULL)
		return (0);

	/*
	 * The only cursor modifications are real, however, the underlying
	 * line may have changed; don't trust anything.  This code has been
	 * a remarkably fertile place for bugs.  Do a reality check on a
	 * cursor value, and make sure it's okay.  If necessary, change it.
	 * Ex keeps track of the line number, but it doesn't care about the
	 * column and it may have disappeared.
	 *
	 * XXX
	 * Ex will soon have to start handling the column; see POSIX 1003.2.
	 */
	if (file_gline(sp, sp->lno, &len) == NULL) {
		if (file_lline(sp, &lno))
			return (1);
		if (lno != 0)
			GETLINE_ERR(sp, sp->lno);
		sp->lno = 1;
		sp->cno = 0;
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
	SVI_PRIVATE *svp;
	size_t oldy, oldx;
	int len, rlen, tlen;
	const char *p, *t;

	/*
	 * XXX
	 * If it's a 4.4BSD system, we could just use fpurge(3).
	 * This shouldn't be too expensive, though.
	 */
	sp = cookie;
	svp = SVP(sp);
	if (INTERRUPTED(sp))
		return (llen);

	p = line;			/* In case of a write of 0. */
	for (rlen = llen; llen;) {
		/* Get the next line. */
		if ((p = memchr(line, '\n', llen)) == NULL)
			len = llen;
		else
			len = p - line;

		/*
		 * The max is sp->cols characters, and we may
		 * have already written part of the line.
		 */
		if (len + svp->exlcontinue > sp->cols)
			len = sp->cols - svp->exlcontinue;

		/*
		 * If the first line output, do nothing.
		 * If the second line output, draw the divider line.
		 * If drew a full screen, remove the divider line.
		 * If it's a continuation line, move to the continuation
		 * point, else, move the screen up.
		 */
		if (svp->exlcontinue == 0) {
			if (svp->extotalcount == 1) {
				MOVE(sp, INFOLINE(sp) - 1, 0);
				clrtoeol();
				if (svi_ex_divider(sp))
					return (-1);
				F_SET(svp, SVI_DIVIDER);
				++svp->extotalcount;
				++svp->exlinecount;
			}
			if (svp->extotalcount == sp->t_maxrows &&
			    F_ISSET(svp, SVI_DIVIDER)) {
				--svp->extotalcount;
				--svp->exlinecount;
				F_CLR(svp, SVI_DIVIDER);
			}
			if (svp->extotalcount != 0 &&
			    svi_ex_scroll(sp, NULL, SCROLL_QUIT))
				return (-1);

			MOVE(sp, INFOLINE(sp), 0);
			++svp->extotalcount;
			++svp->exlinecount;

			if (INTERRUPTED(sp))
				break;
		} else
			MOVE(sp, INFOLINE(sp), svp->exlcontinue);

		/* Display the line, doing character translation. */
		if (F_ISSET(svp, SVI_SCR_IVIDEO))
			standout();
		for (t = line, tlen = len; tlen--; ++t)
			ADDCH(*t);
		if (F_ISSET(svp, SVI_SCR_IVIDEO))
			standend();

		/* Clear to EOL. */
		getyx(stdscr, oldy, oldx);
		if (oldx < sp->cols)
			clrtoeol();

		/* If we loop, it's a new line. */
		svp->exlcontinue = 0;

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
		getyx(stdscr, oldy, svp->exlcontinue);
	return (rlen);
}

/*
 * svi_ex_scroll --
 *	Scroll the screen for ex output.
 */
static int
svi_ex_scroll(sp, chp, flags)
	SCR *sp;
	CH *chp;
	u_int flags;
{
	CH ikey;
	SVI_PRIVATE *svp;

	/*
	 * Scroll the screen.  Instead of scrolling the entire screen, delete
	 * the line above the first line output so preserve the maximum amount
	 * of the screen.
	 */
	svp = SVP(sp);
	MOVE(sp, svp->extotalcount < sp->rows ?
	    INFOLINE(sp) - svp->extotalcount : 0, 0);
	deleteln();

	/* If there are screens below us, push them back into place. */
	if (sp->q.cqe_next != (void *)&sp->gp->dq) {
		MOVE(sp, INFOLINE(sp), 0);
		insertln();
	}

	/* If just displayed a full screen, wait. */
	if (LF_ISSET(SCROLL_WAIT) || svp->exlinecount == sp->t_maxrows) {
		MOVE(sp, INFOLINE(sp), 0);
		if (LF_ISSET(SCROLL_QUIT)) {
			ADDNSTR(STR_QMSG, (int)sizeof(STR_QMSG) - 1);
		} else {
			ADDNSTR(STR_CMSG, (int)sizeof(STR_CMSG) - 1);
		}
		svp->exlinecount = 0;

		clrtoeol();
		refresh();
		/*
		 * !!!
		 * Historic practice is that any key can be used to continue.
		 * Nvi used to require that the user enter a <carriage-return>
		 * or <newline>, but this broke historic users.
		 */
		switch (term_key(sp, &ikey, 0)) {
		case INP_OK:
			if (LF_ISSET(SCROLL_QUIT) && ikey.ch == CH_QUIT)
				F_SET(sp, S_INTERRUPTED);
			if (chp != NULL)
				*chp = ikey;
			break;
		case INP_EOF:
		case INP_ERR:
			return (-1);
			/* NOTREACHED */
		case INP_INTR:
			F_SET(sp, S_INTERRUPTED);
			break;
		}
	}
	return (0);
}

/*
 * svi_ex_inv --
 *	Change whatever is on the info line to inverse video so we have
 *	a divider line between split screens.
 */
static int
svi_ex_inv(sp)
	SCR *sp;
{
	CHAR_T ch;
	size_t spcnt, col, row;

	row = INFOLINE(sp);

	/*
	 * Walk through the line, retrieving each character and writing
	 * it back out in inverse video.  Since curses doesn't have an
	 * EOL marker, only put out trailing spaces if we find another
	 * character.
	 *
	 * XXX
	 * This is a major kluge -- curses should have an interface
	 * that allows us to change attributes on a per line basis.
	 */
	MOVE(sp, row, 0);
	standout();
	for (spcnt = col = 0;;) {
		ch = winch(stdscr);
		if (isspace(ch)) {
			++spcnt;
			if (++col >= sp->cols)
				break;
			MOVE(sp, row, col);
		} else {
			if (spcnt) {
				MOVE(sp, row, col - spcnt);
				for (; spcnt > 0; --spcnt)
					ADDCH(' ');
			}
			ADDCH(ch);
			if (++col >= sp->cols)
				break;
		}
	}
	standend();
	return (0);
}

/*
 * svi_ex_divider --
 *	Draw a dividing line between the screens.
 */
static int
svi_ex_divider(sp)
	SCR *sp;
{
	size_t len;

#define	DIVIDESTR	"+=+=+=+=+=+=+=+"
	len = sizeof(DIVIDESTR) - 1 > sp->cols ?
	    sp->cols : sizeof(DIVIDESTR) - 1;
	standout();
	ADDNSTR(DIVIDESTR, len);
	standend();
	return (0);
}
