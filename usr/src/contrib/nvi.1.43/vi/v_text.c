/*-
 * Copyright (c) 1992, 1993, 1994
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
static char sccsid[] = "@(#)v_text.c	9.6 (Berkeley) 11/16/94";
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

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "vcmd.h"

/*
 * !!!
 * Repeated input in the historic vi is mostly wrong and this isn't very
 * backward compatible.  For example, if the user entered "3Aab\ncd" in
 * the historic vi, the "ab" was repeated 3 times, and the "\ncd" was then
 * appended to the result.  There was also a hack which I don't remember
 * right now, where "3o" would open 3 lines and then let the user fill them
 * in, to make screen movements on 300 baud modems more tolerable.  I don't
 * think it's going to be missed.
 *
 * !!!
 * There's a problem with the way that we do logging for change commands with
 * implied motions (e.g. A, I, O, cc, etc.).  Since the main vi loop logs the
 * starting cursor position before the change command "moves" the cursor, the
 * cursor position to which we return on undo will be where the user entered
 * the change command, not the start of the change.  Several of the following
 * routines re-log the cursor to make this work correctly.  Historic vi tried
 * to do the same thing, and mostly got it right.  (The only spectacular way
 * it fails is if the user entered 'o' from anywhere but the last character of
 * the line, the undo returned the cursor to the start of the line.  If the
 * user was on the last character of the line, the cursor returned to that
 * position.)  We also check for mapped keys waiting, i.e. if we're in the
 * middle of a map, don't bother logging the cursor.
 */
#define	LOG_CORRECT {							\
	if (!MAPPED_KEYS_WAITING(sp))					\
		(void)log_cursor(sp);					\
}
#define	LOG_CORRECT_FIRST {						\
	if (first == 1) {						\
		LOG_CORRECT;						\
		first = 0;						\
	}								\
}

static u_int	set_txt_std __P((SCR *, VICMDARG *, u_int));

/*
 * v_iA -- [count]A
 *	Append text to the end of the line.
 */
int
v_iA(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	size_t len;

	if (file_gline(sp, vp->m_start.lno, &len) != NULL)
		sp->cno = len == 0 ? 0 : len - 1;
	return (v_ia(sp, vp));
}

/*
 * v_ia -- [count]a
 *	   [count]A
 *	Append text to the cursor position.
 */
int
v_ia(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t lno;
	u_long cnt;
	u_int flags;
	size_t len;
	char *p;

	sp->showmode = "Append";
	flags = set_txt_std(sp, vp, 0);
	for (lno = vp->m_start.lno,
	    cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1; cnt--;) {
		/*
		 * Move the cursor one column to the right and
		 * repaint the screen.
		 */
		if ((p = file_gline(sp, lno, &len)) == NULL) {
			if (file_lline(sp, &lno))
				return (1);
			if (lno != 0) {
				GETLINE_ERR(sp, lno);
				return (1);
			}
			lno = 1;
			len = 0;
			LF_SET(TXT_APPENDEOL);
		} else if (len) {
			if (len == sp->cno + 1) {
				sp->cno = len;
				LF_SET(TXT_APPENDEOL);
			} else
				++sp->cno;
		} else
			LF_SET(TXT_APPENDEOL);

		if (v_ntext(sp,
		    sp->tiqp, NULL, p, len, &vp->m_final, 0, OOBLNO, flags))
			return (1);
		LF_CLR(TXT_APPENDEOL);

		LF_SET(TXT_REPLAY);
		sp->lno = lno = vp->m_final.lno;
		sp->cno = vp->m_final.cno;
	}
	return (0);
}

/*
 * v_iI -- [count]I
 *	Insert text at the first nonblank.
 */
int
v_iI(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	sp->cno = 0;
	return (nonblank(sp, vp->m_start.lno, &sp->cno) ? 1 : v_ii(sp, vp));
}

/*
 * v_ii -- [count]i
 *	   [count]I
 *	Insert text at the cursor position.
 */
int
v_ii(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t lno;
	u_long cnt;
	size_t len;
	u_int flags;
	char *p;

	sp->showmode = "Insert";
	flags = set_txt_std(sp, vp, 0);
	for (lno = vp->m_start.lno,
	    cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1; cnt--;) {
		if ((p = file_gline(sp, lno, &len)) == NULL) {
			if (file_lline(sp, &lno))
				return (1);
			if (lno != 0) {
				GETLINE_ERR(sp, vp->m_start.lno);
				return (1);
			}
			lno = 1;
			len = 0;
		}
		/* If len == sp->cno, it's a replay caused by a count. */
		if (len == 0 || len == sp->cno)
			LF_SET(TXT_APPENDEOL);

		if (v_ntext(sp,
		    sp->tiqp, NULL, p, len, &vp->m_final, 0, OOBLNO, flags))
			return (1);
		LF_CLR(TXT_APPENDEOL);

		LF_SET(TXT_REPLAY);
		sp->lno = lno = vp->m_final.lno;
		if ((sp->cno = vp->m_final.cno) != 0)
			++sp->cno;
	}
	return (0);
}

/*
 * v_iO -- [count]O
 *	Insert text above this line.
 */
int
v_iO(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t ai_line, lno;
	size_t len;
	u_long cnt;
	u_int flags;
	int first;
	char *p;

	sp->showmode = "Insert";
	flags = set_txt_std(sp, vp, TXT_APPENDEOL);
	for (first = 1, cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1; cnt--;) {
		if (sp->lno == 1) {
			if (file_lline(sp, &lno))
				return (1);
			if (lno != 0)
				goto insert;
			p = NULL;
			len = 0;
			ai_line = OOBLNO;
		} else {
insert:			p = "";
			sp->cno = 0;

			/* Correct logging for implied cursor motion. */
			LOG_CORRECT_FIRST;

			if (file_iline(sp, sp->lno, p, 0))
				return (1);
			if ((p = file_gline(sp, sp->lno, &len)) == NULL) {
				GETLINE_ERR(sp, sp->lno);
				return (1);
			}
			ai_line = sp->lno + 1;
		}

		if (v_ntext(sp,
		    sp->tiqp, NULL, p, len, &vp->m_final, 0, ai_line, flags))
			return (1);

		LF_SET(TXT_REPLAY);
		sp->lno = lno = vp->m_final.lno;
		sp->cno = vp->m_final.cno;
	}
	return (0);
}

/*
 * v_io -- [count]o
 *	Insert text after this line.
 */
int
v_io(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t ai_line, lno;
	size_t len;
	u_long cnt;
	u_int flags;
	int first;
	char *p;

	sp->showmode = "Insert";
	flags = set_txt_std(sp, vp, TXT_APPENDEOL);
	for (first = 1,
	    cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1; cnt--;) {
		if (sp->lno == 1) {
			if (file_lline(sp, &lno))
				return (1);
			if (lno != 0)
				goto insert;
			p = NULL;
			len = 0;
			ai_line = OOBLNO;
		} else {
insert:			p = "";
			sp->cno = 0;

			/* Correct logging for implied cursor motion. */
			LOG_CORRECT_FIRST;

			len = 0;
			if (file_aline(sp, 1, sp->lno, p, len))
				return (1);
			if ((p = file_gline(sp, ++sp->lno, &len)) == NULL) {
				GETLINE_ERR(sp, sp->lno);
				return (1);
			}
			ai_line = sp->lno - 1;
		}

		if (v_ntext(sp,
		    sp->tiqp, NULL, p, len, &vp->m_final, 0, ai_line, flags))
			return (1);

		LF_SET(TXT_REPLAY);
		sp->lno = lno = vp->m_final.lno;
		sp->cno = vp->m_final.cno;
	}
	return (0);
}

/*
 * v_change -- [buffer][count]c[count]motion
 *	       [buffer][count]C
 *	       [buffer][count]S
 *	Change command.
 */
int
v_change(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t lno;
	size_t blen, len;
	u_int flags;
	int lmode, rval;
	char *bp, *p;

	/*
	 * Find out if the file is empty, it's easier to handle it as a
	 * special case.
	 */
	if (vp->m_start.lno == vp->m_stop.lno &&
	    (p = file_gline(sp, vp->m_start.lno, &len)) == NULL) {
		if (file_lline(sp, &lno))
			return (1);
		if (lno != 0) {
			GETLINE_ERR(sp, vp->m_start.lno);
			return (1);
		}
		return (v_ia(sp, vp));
	}

	sp->showmode = "Change";
	flags = set_txt_std(sp, vp, 0);

	/*
	 * Move the cursor to the start of the change.  Note, if autoindent
	 * is turned on, the cc command in line mode changes from the first
	 * *non-blank* character of the line, not the first character.  And,
	 * to make it just a bit more exciting, the initial space is handled
	 * as auto-indent characters.
	 */
	lmode = F_ISSET(vp, VM_LMODE) ? CUT_LINEMODE : 0;
	if (lmode) {
		vp->m_start.cno = 0;
		if (O_ISSET(sp, O_AUTOINDENT)) {
			if (nonblank(sp, vp->m_start.lno, &vp->m_start.cno))
				return (1);
			LF_SET(TXT_AICHARS);
		}
	}
	sp->lno = vp->m_start.lno;
	sp->cno = vp->m_start.cno;

	/* Correct logging for implied cursor motion. */
	LOG_CORRECT;

	/*
	 * 'c' can be combined with motion commands that set the resulting
	 * cursor position, i.e. "cG".  Clear the VM_RCM flags and make the
	 * resulting cursor position stick, inserting text has its own rules
	 * for cursor positioning.
	 */
	F_CLR(vp, VM_RCM_MASK);
	F_SET(vp, VM_RCM_SET);

	/*
	 * If not in line mode and changing within a single line, copy the
	 * text and overwrite it.
	 */
	if (!lmode && vp->m_start.lno == vp->m_stop.lno) {
		/*
		 * !!!
		 * Historic practice, c did not cut into the numeric buffers,
		 * only the unnamed one.
		 */
		if (cut(sp,
		    F_ISSET(vp, VC_BUFFER) ? &vp->buffer : NULL,
		    &vp->m_start, &vp->m_stop, lmode))
			return (1);
		if (len == 0)
			LF_SET(TXT_APPENDEOL);
		LF_SET(TXT_EMARK | TXT_OVERWRITE);
		return (v_ntext(sp, sp->tiqp,
		    &vp->m_stop, p, len, &vp->m_final, 0, OOBLNO, flags));
	}

	/*
	 * It's trickier if in line mode or changing over multiple lines.  If
	 * we're in line mode delete all of the lines and insert a replacement
	 * line which the user edits.  If there was leading whitespace in the
	 * first line being changed, we copy it and use it as the replacement.
	 * If we're not in line mode, we delete the text and start inserting.
	 *
	 * !!!
	 * Copy the text.  Historic practice, c did not cut into the numeric
	 * buffers, only the unnamed one.
	 */
	if (cut(sp,
	    F_ISSET(vp, VC_BUFFER) ? &vp->buffer : NULL,
	    &vp->m_start, &vp->m_stop, lmode))
		return (1);

	/* If replacing entire lines and there's leading text. */
	if (lmode && vp->m_start.cno) {
		/* Get a copy of the first line changed. */
		if ((p = file_gline(sp, vp->m_start.lno, &len)) == NULL) {
			GETLINE_ERR(sp, vp->m_start.lno);
			return (1);
		}
		/* Copy the leading text elsewhere. */
		GET_SPACE_RET(sp, bp, blen, vp->m_start.cno);
		memmove(bp, p, vp->m_start.cno);
	} else
		bp = NULL;

	/* Delete the text. */
	if (delete(sp, &vp->m_start, &vp->m_stop, lmode))
		return (1);

	/* If replacing entire lines, insert a replacement line. */
	if (lmode) {
		if (file_iline(sp, vp->m_start.lno, bp, vp->m_start.cno))
			return (1);
		sp->lno = vp->m_start.lno;
		len = sp->cno = vp->m_start.cno;
	}

	/* Get the line we're editing. */
	if ((p = file_gline(sp, vp->m_start.lno, &len)) == NULL) {
		if (file_lline(sp, &lno))
			return (1);
		if (lno != 0) {
			GETLINE_ERR(sp, vp->m_start.lno);
			return (1);
		}
		len = 0;
	}

	/* Check to see if we're appending to the line. */
	if (vp->m_start.cno >= len)
		LF_SET(TXT_APPENDEOL);

	rval = v_ntext(sp,
	    sp->tiqp, NULL, p, len, &vp->m_final, 0, OOBLNO, flags);

	if (bp != NULL)
		FREE_SPACE(sp, bp, blen);
	return (rval);
}

/*
 * v_Replace -- [count]R
 *	Overwrite multiple characters.
 */
int
v_Replace(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t lno;
	u_long cnt;
	size_t len;
	u_int flags;
	char *p;

	sp->showmode = "Replace";
	flags = set_txt_std(sp, vp, 0);

	cnt = F_ISSET(vp, VC_C1SET) ? vp->count : 1;
	if ((p = file_gline(sp, vp->m_start.lno, &len)) == NULL) {
		if (file_lline(sp, &lno))
			return (1);
		if (lno != 0) {
			GETLINE_ERR(sp, vp->m_start.lno);
			return (1);
		}
		len = 0;
		LF_SET(TXT_APPENDEOL);
	} else {
		if (len == 0)
			LF_SET(TXT_APPENDEOL);
		LF_SET(TXT_OVERWRITE | TXT_REPLACE);
	}
	vp->m_stop.lno = vp->m_start.lno;
	vp->m_stop.cno = len ? len - 1 : 0;
	if (v_ntext(sp, sp->tiqp,
	    &vp->m_stop, p, len, &vp->m_final, 0, OOBLNO, flags))
		return (1);
	LF_CLR(TXT_APPENDEOL | TXT_OVERWRITE | TXT_REPLACE);

	/*
	 * Special case.  The historic vi handled [count]R badly, in that R
	 * would replace some number of characters, and then the count would
	 * append count-1 copies of the replacing chars to the replaced space.
	 * This seems wrong, so this version counts R commands.  There is some
	 * trickiness in moving back to where the user stopped replacing after
	 * each R command.  Basically, if the user ended with a newline, we
	 * want to use vp->m_final.cno (which will be 0).  Otherwise, use the
	 * column after the returned cursor, unless it would be past the end of
	 * the line, in which case we append to the line.
	 */
	while (--cnt) {
		if ((p = file_gline(sp, vp->m_final.lno, &len)) == NULL)
			GETLINE_ERR(sp, vp->m_final.lno);
		LF_SET(TXT_REPLAY);

		sp->lno = vp->m_final.lno;

		if (len == 0 || vp->m_final.cno == len - 1) {
			sp->cno = len;
			LF_SET(TXT_APPENDEOL);
		} else {
			sp->cno = vp->m_final.cno;
			if (vp->m_final.cno != 0)
				++sp->cno;
			LF_SET(TXT_OVERWRITE | TXT_REPLACE);
		}

		vp->m_stop.lno = sp->lno;
		vp->m_stop.cno = sp->cno;
		if (v_ntext(sp, sp->tiqp,
		    &vp->m_stop, p, len, &vp->m_final, 0, OOBLNO, flags))
			return (1);
		LF_CLR(TXT_APPENDEOL | TXT_OVERWRITE | TXT_REPLACE);
	}
	return (0);
}

/*
 * v_subst -- [buffer][count]s
 *	Substitute characters.
 */
int
v_subst(sp, vp)
	SCR *sp;
	VICMDARG *vp;
{
	recno_t lno;
	size_t len;
	u_int flags;
	char *p;

	sp->showmode = "Change";
	flags = set_txt_std(sp, vp, 0);
	if ((p = file_gline(sp, vp->m_start.lno, &len)) == NULL) {
		if (file_lline(sp, &lno))
			return (1);
		if (lno != 0) {
			GETLINE_ERR(sp, vp->m_start.lno);
			return (1);
		}
		len = 0;
		LF_SET(TXT_APPENDEOL);
	} else {
		if (len == 0)
			LF_SET(TXT_APPENDEOL);
		LF_SET(TXT_EMARK | TXT_OVERWRITE);
	}

	vp->m_stop.lno = vp->m_start.lno;
	vp->m_stop.cno =
	    vp->m_start.cno + (F_ISSET(vp, VC_C1SET) ? vp->count - 1 : 0);
	if (vp->m_stop.cno > len - 1)
		vp->m_stop.cno = len - 1;

	if (p != NULL && cut(sp,
	    F_ISSET(vp, VC_BUFFER) ? &vp->buffer : NULL,
	    &vp->m_start, &vp->m_stop, 0))
		return (1);

	return (v_ntext(sp, sp->tiqp,
	    &vp->m_stop, p, len, &vp->m_final, 0, OOBLNO, flags));
}

/*
 * set_txt_std --
 *	Initialize text processing flags.
 */
static u_int
set_txt_std(sp, vp, init)
	SCR *sp;
	VICMDARG *vp;
	u_int init;
{
	u_int flags;

	LF_INIT(init);
	LF_SET(TXT_CNTRLT |
	    TXT_ESCAPE | TXT_MAPINPUT | TXT_RECORD | TXT_RESOLVE);
	if (O_ISSET(sp, O_ALTWERASE))
		LF_SET(TXT_ALTWERASE);
	if (O_ISSET(sp, O_AUTOINDENT))
		LF_SET(TXT_AUTOINDENT);
	if (O_ISSET(sp, O_BEAUTIFY))
		LF_SET(TXT_BEAUTIFY);
	if (O_ISSET(sp, O_SHOWMATCH))
		LF_SET(TXT_SHOWMATCH);
	if (F_ISSET(sp, S_SCRIPT))
		LF_SET(TXT_CR);
	if (O_ISSET(sp, O_TTYWERASE))
		LF_SET(TXT_TTYWERASE);
	if (F_ISSET(vp,  VC_ISDOT))
		LF_SET(TXT_REPLAY);

	/*
	 * !!!
	 * Mapped keys were sometimes unaffected by the wrapmargin option
	 * in the historic 4BSD vi.  Consider the following commands, where
	 * each is executed on an empty line, in an 80 column screen, with
	 * the wrapmargin value set to 60.
	 *
	 *	aABC DEF <ESC>....
	 *	:map K aABC DEF ^V<ESC><CR>KKKKK
	 *	:map K 5aABC DEF ^V<ESC><CR>K
	 * 
	 * The first and second commands are affected by wrapmargin.  The
	 * third is not.  (If the inserted text is itself longer than the
	 * wrapmargin value, i.e. if the "ABC DEF " string is replaced by
	 * something that's longer than 60 columns from the beginning of
	 * the line, the first two commands behave as before, but the third
	 * command gets fairly strange.)  The problem is that people wrote
	 * macros that depended on the third command NOT being affected by
	 * wrapmargin, as in this gem which centers lines:
	 *
	 *	map #c $mq81a ^V^[81^V^V|D`qld0:s/  / /g^V^M$p
	 *
	 * For compatibility reasons, we try and make it all work here.  I
	 * offer no hope that this is right, but it's probably pretty close.
	 */
	if ((O_ISSET(sp, O_WRAPLEN) || O_ISSET(sp, O_WRAPMARGIN)) &&
	    (!MAPPED_KEYS_WAITING(sp) || !F_ISSET(vp, VC_C1SET)))
		LF_SET(TXT_WRAPMARGIN);
	return (flags);
}
