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
static char sccsid[] = "@(#)v_ntext.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"

static int	 txt_abbrev __P((SCR *, TEXT *));
static TEXT	*txt_backup __P((SCR *, EXF *, HDR *, TEXT *, u_int));
static void	 txt_err __P((SCR *, EXF *, HDR *));
static int	 txt_indent __P((SCR *, TEXT *));
static int	 txt_outdent __P((SCR *, TEXT *));
static int	 txt_resolve __P((SCR *, EXF *, HDR *));

/* Cursor character (space is hard to track on the screen). */
#ifdef DEBUG
#define	CURSOR_CHAR	'+'
#else
#define	CURSOR_CHAR	' '
#endif

/* Error jump. */
#define	ERR {								\
	eval = 1;							\
	txt_err(sp, ep, hp);						\
	goto ret;							\
}

/* Local version of BINC. */
#define	TBINC(sp, lp, llen, nlen) {					\
	if ((nlen) > llen && binc(sp, &(lp), &(llen), nlen))		\
		ERR;							\
}

/*
 * newtext --
 *	Read in text from the user.
 */
int
v_ntext(sp, ep, hp, tm, p, len, rp, prompt, ai_line, flags)
	SCR *sp;
	EXF *ep;
	HDR *hp;
	MARK *tm;		/* To MARK. */
	char *p;		/* Input line. */
	size_t len;		/* Input line length. */
	MARK *rp;		/* Return MARK. */
	int prompt;		/* Prompt to display. */
	recno_t ai_line;	/* Line number to use for autoindent count. */
	u_int flags;		/* TXT_ flags. */
{
				/* State of the "[^0]^D" sequences. */
	enum { C_CARATSET, C_NOCHANGE, C_NOTSET, C_ZEROSET } carat_st;
				/* State of quotation. */
	enum { Q_NOTSET, Q_NEXTCHAR, Q_THISCHAR } quoted;
				/* State of abbreviation checks. */
	enum { L_NOCHECK, L_SPACE, L_NOTSPACE } lch;
	TEXT *tp, *ntp;		/* Input text structures. */
	TEXT *aitp;		/* Autoindent text structure. */
	size_t rcol;		/* 0-N: insert offset in the replay buffer. */
	int ch;			/* Input character. */
	int eval;		/* Routine return value. */
	int replay;		/* If replaying a set of input. */
	int tty_cwait;		/* Characters waiting. */
	int max, tmp;

	/* Set the input flag, so tabs get displayed correctly. */
	F_SET(sp, S_INPUT);

	/* Set text buffer in-use flag. */
	F_SET(hp, HDR_INUSE);

	/* Set return value. */
	eval = 0;

	/*
	 * Get one TEXT structure with some initial buffer space, reusing
	 * the last one if it's big enough.  (All TEXT bookkeeping fields
	 * default to 0 -- text_init() handles this.)  If changing a line,
	 * copy it into the TEXT buffer.
	 */
	if (hp->next != hp) {
		tp = hp->next;
		if (tp->next != (TEXT *)hp || tp->lb_len < len + 32) {
			hdr_text_free(hp);
			goto newtp;
		}
		tp->ai = tp->insert = tp->offset = tp->overwrite = 0;
		if (p != NULL) {
			tp->len = len;
			memmove(tp->lb, p, len);
		} else
			tp->len = 0;
	} else {
newtp:		if ((tp = text_init(sp, p, len, len + 32)) == NULL)
			return (1);
		HDR_INSERT(tp, hp, next, prev, TEXT);
	}

	/* Set the starting line number. */
	tp->lno = sp->lno;

	/*
	 * Set the insert and overwrite counts.  If overwriting characters,
	 * do insertion afterward.  If not overwriting characters, assume
	 * doing insertion.  If change is to a mark, emphasize it with an
	 * END_CH.
	 */
	if (len) {
		if (LF_ISSET(TXT_OVERWRITE)) {
			tp->overwrite = tm->cno - sp->cno;
			tp->insert = len - tm->cno;
		} else
			tp->insert = len - sp->cno;

		if (LF_ISSET(TXT_EMARK))
			tp->lb[tm->cno - 1] = END_CH;
	}

	/*
	 * Many of the special cases in this routine are to handle autoindent
	 * support.  Some utter fool decided that it would be a good idea if
	 * "^^D" and "0^D" deleted all of the autoindented characters.  In an
	 * editor that takes single character input from the user, this is so
	 * stupid as to be unbelievable.  Note also that "^^D" resets the next
	 * lines' autoindent, but "0^D" doesn't.
	 *
	 * We assume that autoindent only happens on empty lines, so insert
	 * and overwrite will be zero.  If doing autoindent, figure out how
	 * much indentation we need and fill it in.  Update input column and
	 * screen cursor as necessary.
	 */
	if (LF_ISSET(TXT_AUTOINDENT) && ai_line != OOBLNO) {
		if (txt_auto(sp, ep, ai_line, NULL, tp))
			return (1);
		sp->cno = tp->ai;
	} else {
		/*
		 * The change command has a special "feature" -- leading space
		 * characters are handled as autoindent characters.  Beauty!
		 */
		if (LF_ISSET(TXT_AICHARS)) {
			tp->offset = 0;
			tp->ai = sp->cno;
		} else
			tp->offset = sp->cno;
	}
	aitp = tp;

	/* If getting a command buffer from the user, there may be a prompt. */
	if (LF_ISSET(TXT_PROMPT)) {
		tp->lb[sp->cno++] = prompt;
		++tp->len;
		++tp->offset;
	}

	/*
	 * If appending after the end-of-line, add a space into the buffer
	 * and move the cursor right.  This space is inserted, i.e. pushed
	 * along, and then deleted when the line is resolved.  Assumes that
	 * the cursor is already positioned at the end of the line.  This
	 * avoids the nastiness of having the cursor reside on a magical
	 * column, i.e. a column that doesn't really exist.  The only down
	 * side is that we may wrap lines or scroll the screen before it's
	 * strictly necessary.  Not a big deal.
	 */
	if (LF_ISSET(TXT_APPENDEOL)) {
		tp->lb[sp->cno] = CURSOR_CHAR;
		++tp->len;
		++tp->insert;
	}

	/*
	 * Set up the dot command.  Dot commands are done by saving the
	 * actual characters and replaying the input.
	 *
	 * XXX
	 * It would be nice if we could swallow backspaces and such, but
	 * it's not all that easy to do.  Another possibility would be to
	 * recognize full line insertions, which could be performed quickly,
	 * without replay.
	 */
	rcol = 0;
	replay = LF_ISSET(TXT_REPLAY);

	/* Initialize abbreviations check. */
	lch = F_ISSET(sp, S_ABBREV) &&
	    LF_ISSET(TXT_MAPINPUT) ? L_NOTSPACE : L_NOCHECK;

	for (carat_st = C_NOTSET, quoted = Q_NOTSET, tty_cwait = 0;;) {

		/* Reset the line and update the screen. */
		if (sp->s_change(sp, ep, tp->lno, LINE_RESET))
			ERR;
		/* Three chosen by random selection. */
		if (tty_cwait > 3 || !term_waiting(sp)) {
			tty_cwait = 0;
			if (sp->s_refresh(sp, ep))
				ERR;
		} else
			++tty_cwait;
		
next_ch:	if (replay)
			ch = sp->rep[rcol++];
		else {
			/* Get the character. */
			ch = term_key(sp, flags & TXT_GETKEY_MASK);

			/*
			 * Check if the character fits into the input and
			 * replay buffers; allocate space as necesssary.
			 * It's not necessary to check tp->len, since it
			 * doesn't include the overwrite characters, but
			 * it's not worth fixing it.
			 */
			TBINC(sp, tp->lb, tp->lb_len, tp->len + 1);

			if (LF_ISSET(TXT_RECORD)) {
				/* Store the character into replay buffer. */
				TBINC(sp, sp->rep, sp->rep_len, rcol + 1);
				sp->rep[rcol++] = ch;
			}
		}

		/*
		 * If the character was quoted, replace the last
		 * character (the literal mark) with the new character.
		 */
		if (quoted == Q_THISCHAR) {
			--sp->cno;
			++tp->overwrite;
			quoted = Q_NOTSET;
			goto ins_ch;
		}

		switch (sp->special[ch]) {
		case K_CR:
		case K_NL:				/* New line. */
#define	LINE_RESOLVE {							\
			/* Handle abbreviations. */			\
			if (lch == L_NOTSPACE && txt_abbrev(sp, tp))	\
				ERR;					\
			if (lch != L_NOCHECK)				\
				lch = L_SPACE;				\
			/*						\
			 * The "R" command doesn't delete characters	\
			 * that it could have overwritten.  Other input	\
			 * modes do.					\
			 */						\
			if (LF_ISSET(TXT_REPLACE)) {			\
				tp->insert += tp->overwrite;		\
				tp->overwrite = 0;			\
			}						\
			/* Delete any appended cursor. */		\
			if (LF_ISSET(TXT_APPENDEOL)) {			\
				--tp->len;				\
				--tp->insert;				\
			}						\
			/*						\
			 * If the user has not inserted any characters	\
			 * and there aren't any other characters in the	\
			 * line, delete the autoindent characters.	\
			 */						\
			if (LF_ISSET(TXT_AUTOINDENT) &&			\
			    !tp->insert && sp->cno <= tp->ai) {		\
				tp->len = tp->overwrite = 0;		\
				sp->cno = 0;				\
			}						\
}
			LINE_RESOLVE;

			if (LF_ISSET(TXT_CR))
				goto k_escape;

			/*
			 * Move any remaining insert characters into
			 * a new TEXT structure.
			 */
			if ((ntp = text_init(sp,
			    tp->lb + sp->cno + tp->overwrite,
			    tp->insert, tp->insert + 32)) == NULL)
				ERR;
			HDR_INSERT(ntp, hp, next, prev, TEXT);

			/* Set bookkeeping for the new line. */
			ntp->lno = tp->lno + 1;
			ntp->insert = tp->insert;

			/*
			 * Reset the autoindent line.  0^D keeps the ai
			 * line from changing, ^D changes the level, even
			 * if no characters in the line.
			 */
			if (LF_ISSET(TXT_AUTOINDENT)) {
				if (carat_st != C_NOCHANGE)
					aitp = tp;
				if (txt_auto(sp, ep, OOBLNO, aitp, ntp))
					ERR;
				carat_st = C_NOTSET;
			}
			
			/* Can now reset bookkeeping for the old line. */
			tp->len = sp->cno;
			tp->ai = tp->insert = tp->overwrite = 0;

			/* New cursor position. */
			sp->cno = ntp->ai;

			/* New lines are TXT_APPENDEOL if nothing to insert. */
			if (ntp->insert == 0) {
				TBINC(sp, tp->lb, tp->lb_len, tp->len + 1);
				LF_SET(TXT_APPENDEOL);
				ntp->lb[sp->cno] = CURSOR_CHAR;
				++ntp->insert;
				++ntp->len;
			}

			/* Update the old line. */
			if (sp->s_change(sp, ep, tp->lno, LINE_RESET))
				ERR;

			/* Swap old and new TEXT's. */
			tp = ntp;

			/* Reset the cursor. */
			sp->lno = tp->lno;

			/* Update the new line. */
			if (sp->s_change(sp, ep, tp->lno, LINE_INSERT) ||
			    sp->s_refresh(sp, ep))
				ERR;

			goto next_ch;
		case K_ESCAPE:				/* Escape. */
			if (!LF_ISSET(TXT_ESCAPE))
				goto ins_ch;

			LINE_RESOLVE;

			/* If there are insert characters, copy them down. */
k_escape:		if (tp->insert && tp->overwrite)
				memmove(tp->lb + sp->cno,
				    tp->lb + sp->cno + tp->overwrite,
				    tp->insert);
			tp->len -= tp->overwrite;

			/*
			 * Delete any lines that were inserted into the text
			 * structure and then erased.
			 */
			while (tp->next != (TEXT *)hp) {
				HDR_DELETE(tp->next, next, prev, TEXT);
				text_free(tp->next);
			}

			/*
			 * If not resolving the lines into the file, end
			 * it with a nul.
			 */
			if (LF_ISSET(TXT_RESOLVE)) {
				if (txt_resolve(sp, ep, hp))
					ERR;
			} else {
				TBINC(sp, tp->lb, tp->lb_len, tp->len + 1);
				tp->lb[tp->len] = '\0';
			}

			/*
			 * Set the return cursor position to rest on the last
			 * inserted character.
			 */
			if (rp != NULL) {
				rp->lno = tp->lno;
				rp->cno = sp->cno ? sp->cno - 1 : 0;
				if (sp->s_change(sp, ep, rp->lno, LINE_RESET))
					ERR;
			}
			goto ret;
		case K_CARAT:			/* Delete autoindent chars. */
			if (LF_ISSET(TXT_AUTOINDENT) && sp->cno <= tp->ai)
				carat_st = C_CARATSET;
			goto ins_ch;
		case K_ZERO:			/* Delete autoindent chars. */
			if (LF_ISSET(TXT_AUTOINDENT) && sp->cno <= tp->ai)
				carat_st = C_ZEROSET;
			goto ins_ch;
		case K_CNTRLD:			/* Delete autoindent char. */
			if (!LF_ISSET(TXT_AUTOINDENT))
				goto ins_ch;
			switch (carat_st) {
			case C_CARATSET:	/* ^^D */
				if (sp->cno == 0 || sp->cno > tp->ai + 1)
					goto ins_ch;
				carat_st = C_NOTSET;
				tp->overwrite += sp->cno;
				tp->ai = sp->cno = 0;
				break;
			case C_ZEROSET:		/* 0^D */
				if (sp->cno == 0 || sp->cno > tp->ai + 1)
					goto ins_ch;
				carat_st = C_NOCHANGE;
				tp->overwrite += sp->cno;
				tp->ai = sp->cno = 0;
				break;
			case C_NOTSET:		/* ^D */
				if (sp->cno == 0 || sp->cno > tp->ai)
					goto ins_ch;
				(void)txt_outdent(sp, tp);
				break;
			default:
				abort();
			}
			break;
		case K_CNTRLT:			/* Add autoindent char. */
			if (!LF_ISSET(TXT_CNTRLT) || sp->cno > tp->ai)
				goto ins_ch;
			if (txt_indent(sp, tp))
				break;
			break;
		case K_VERASE:			/* Erase the last character. */
			/* If can erase over the prompt, return. */
			if (LF_ISSET(TXT_BS) && sp->cno <= tp->offset) {
				tp->len = tp->offset;
				goto ret;
			}

			/*
			 * If at the beginning of the line, try and drop back
			 * to a previously inserted line.
			 */
			if (sp->cno == 0) {
				if ((ntp =
				    txt_backup(sp, ep, hp, tp, flags)) == NULL)
					ERR;
				tp = ntp;
				break;
			}

			/* If nothing to erase, bell the user. */
			if (sp->cno <= tp->offset) {
				msgq(sp, M_BERR,
				    "No more characters to erase.");
				break;
			}

			/* Drop back one character. */
			--sp->cno;

			/* Increment overwrite, decrement ai if deleted. */
			++tp->overwrite;
			if (sp->cno < tp->ai)
				--tp->ai;
			break;
		case K_VWERASE:			/* Skip back one word. */
			/*
			 * If at the beginning of the line, try and drop back
			 * to a previously inserted line.
			 */
			if (sp->cno == 0) {
				if ((ntp =
				    txt_backup(sp, ep, hp, tp, flags)) == NULL)
					ERR;
				tp = ntp;
			}

			/*
			 * If at offset, nothing to erase so bell the user.
			 */
			if (sp->cno <= tp->offset) {
				msgq(sp, M_BERR,
				    "No more characters to erase.");
				break;
			}

			/*
			 * First werase goes back to any autoindent
			 * and second werase goes back to the offset.
			 */
			if (tp->ai && sp->cno > tp->ai)
				max = tp->ai;
			else
				max = tp->offset;

			/* Skip over trailing space characters. */
			while (sp->cno > max && isspace(tp->lb[sp->cno - 1])) {
				--sp->cno;
				++tp->overwrite;
			}
			if (sp->cno == max)
				break;
			for (tmp =
			    inword(tp->lb[sp->cno - 1]); sp->cno > max;) {
				--sp->cno;
				++tp->overwrite;
				if (tmp != inword(tp->lb[sp->cno - 1]) ||
				    isspace(tp->lb[sp->cno - 1]))
					break;
			}
			break;
		case K_VKILL:			/* Restart this line. */
			/*
			 * If at the beginning of the line, try and drop back
			 * to a previously inserted line.
			 */
			if (sp->cno == 0) {
				if ((ntp =
				    txt_backup(sp, ep, hp, tp, flags)) == NULL)
					ERR;
				tp = ntp;
			}

			/* If at offset, nothing to erase so bell the user. */
			if (sp->cno <= tp->offset) {
				msgq(sp, M_BERR,
				    "No more characters to erase.");
				break;
			}

			/*
			 * First kill goes back to any autoindent
			 * and second kill goes back to the offset.
			 */
			if (tp->ai && sp->cno > tp->ai)
				max = tp->ai;
			else
				max = tp->offset;
			tp->overwrite += sp->cno - max;
			sp->cno = max;
			break;
		case K_CNTRLZ:
			(void)sp->s_suspend(sp);
			break;
		case K_FORMFEED:
			F_SET(sp, S_REFRESH);
			break;
		case K_VLNEXT:			/* Quote the next character. */
			ch = '^';
			quoted = Q_NEXTCHAR;
			/* FALLTHROUGH */
		default:			/* Insert the character. */
			/*
			 * If entering a space character after a word, check
			 * for abbreviations.
			 */
ins_ch:			if (isspace(ch) &&
			    lch == L_NOTSPACE && txt_abbrev(sp, tp))
				ERR;
			if (lch != L_NOCHECK)
				lch = isspace(ch) ? L_SPACE : L_NOTSPACE;

			if (tp->overwrite)	/* Overwrite a character. */
				--tp->overwrite;
			else if (tp->insert) {	/* Insert a character. */
				++tp->len;
				if (tp->insert == 1)
					tp->lb[sp->cno + 1] = tp->lb[sp->cno];
				else
					memmove(tp->lb + sp->cno + 1,
					    tp->lb + sp->cno, tp->insert);
			}

			tp->lb[sp->cno++] = ch;

			/*
			 * If we've reached the end of the buffer, then we
			 * need to switch into insert mode.  This happens
			 * when there's a change to a mark and the user puts
			 * in more characters than the length of the motion.
			 */
			if (sp->cno >= tp->len) {
				TBINC(sp, tp->lb, tp->lb_len, tp->len + 1);
				LF_SET(TXT_APPENDEOL);
				tp->lb[sp->cno] = CURSOR_CHAR;
				++tp->insert;
				++tp->len;
			}

			if (quoted == Q_NEXTCHAR)
				quoted = Q_THISCHAR;
			break;
		}
#if DEBUG && 1
		if (sp->cno + tp->insert + tp->overwrite != tp->len)
			msgq(sp, M_ERR,
			    "len %u != cno: %u ai: %u insert %u overwrite %u",
			    tp->len, sp->cno, tp->ai, tp->insert,
			    tp->overwrite);
		tp->len = sp->cno + tp->insert + tp->overwrite;
#endif
	}

	/* Clear input, text buffer in-use flags. */
ret:	F_CLR(sp, S_INPUT);
	F_CLR(hp, HDR_INUSE);

	return (eval);
}

/*
 * txt_abbrev --
 *	Handle abbreviations.
 */
static int
txt_abbrev(sp, tp)
	SCR *sp;
	TEXT *tp;
{
	SEQ *qp;
	size_t diff, len, off;
	char *p;	

	/* Find the beginning of this "word". */
	for (off = sp->cno - 1, p = tp->lb + off, len = 0;; --p, --off) {
		if (isspace(*p)) {
			++p;
			break;
		}
		++len;
		if (off == tp->ai || off == tp->offset)
			break;
	}

	/* Check for any abbreviations. */
	if ((qp = seq_find(sp, p, len, SEQ_ABBREV, NULL)) == NULL)
		return (0);

	/* Copy up or down, depending on the lengths. */
	if (len < qp->olen) {
		diff = qp->olen - len;
		BINC(sp, tp->lb, tp->lb_len, tp->len + diff);
		memmove(tp->lb + sp->cno + diff, tp->lb + sp->cno, tp->insert);
		sp->cno += diff;
		tp->len += diff;
	} else if (len > qp->olen) {
		diff = len - qp->olen;
		memmove(tp->lb + sp->cno - diff, tp->lb + sp->cno, tp->insert);
		sp->cno -= diff;
		tp->len -= diff;
	}
	memmove(p, qp->output, qp->olen);
	return (0);
}

/*
 * txt_auto --
 *	Handle autoindent.  If aitp isn't NULL, use it, otherwise,
 *	retrieve the line.
 */
int
txt_auto(sp, ep, lno, aitp, tp)
	SCR *sp;
	EXF *ep;
	recno_t lno;
	TEXT *aitp, *tp;
{
	size_t len, nlen;
	char *p, *t;
	
	if (aitp == NULL) {
		if ((p = t = file_gline(sp, ep, lno, &len)) == NULL)
			return (0);
	} else {
		len = aitp->len ? aitp->len : aitp->ai;
		p = t = aitp->lb;
	}
	for (nlen = 0; len; ++p) {
		if (!isspace(*p))
			break;
		/* If last character is a space, it counts. */
		if (--len == 0) {
			++p;
			break;
		}
	}

	/* No indentation. */
	if (p == t)
		return (0);

	/* Set count. */
	nlen = p - t;

	/* Make sure the buffer's big enough. */
	BINC(sp, tp->lb, tp->lb_len, tp->len + nlen);

	/* Copy the indentation into the new buffer. */
	memmove(tp->lb + nlen, tp->lb, tp->len);
	memmove(tp->lb, t, nlen);
	tp->len += nlen;

	/* Return the additional length. */
	tp->ai = nlen;
	return (0);
}

/*
 * txt_backup --
 *	Back up to the previously edited line.
 */
static TEXT *
txt_backup(sp, ep, hp, tp, flags)
	SCR *sp;
	EXF *ep;
	HDR *hp;
	TEXT *tp;
	u_int flags;
{
	TEXT *ntp;
	size_t col;

	if (tp->prev == (TEXT *)hp) {
		msgq(sp, M_BERR, "Already at the beginning of the insert");
		return (tp);
	}

	/* Update the old line on the screen. */
	if (sp->s_change(sp, ep, tp->lno, LINE_DELETE))
		return (NULL);

	/* Get a handle on the previous TEXT structure. */
	ntp = tp->prev;

	/* Make sure that we can get enough space. */
	if (LF_ISSET(TXT_APPENDEOL) && ntp->len + 1 > ntp->lb_len &&
	    binc(sp, &ntp->lb, &ntp->lb_len, ntp->len + 1))
		return (NULL);

	/*
	 * Release current TEXT; now committed to the swap, nothing
	 * better fail.
	 */
	HDR_DELETE(tp, next, prev, TEXT);
	text_free(tp);

	/* Swap TEXT's. */
	tp = ntp;

	/* Set bookkeeping information. */
	col = tp->len;
	if (LF_ISSET(TXT_APPENDEOL)) {
		tp->lb[col] = CURSOR_CHAR;
		++tp->insert;
		++tp->len;
	}
	sp->lno = tp->lno;
	sp->cno = col;
	return (tp);
}

/*
 * txt_err --
 *	Handle an error during input processing.
 */
static void
txt_err(sp, ep, hp)
	SCR *sp;
	EXF *ep;
	HDR *hp;
{
	recno_t lno;
	size_t len;

	/*
	 * The problem with input processing is that the cursor is at an
	 * indeterminate position since some input may have been lost due
	 * to a malloc error.  So, try to go back to the place from which
	 * the cursor started, knowing that it may no longer be available.
	 *
	 * We depend on at least one line number being set in the text
	 * chain.
	 */
	for (lno = ((TEXT *)(hp->next))->lno;
	    file_gline(sp, ep, lno, &len) == NULL && lno > 0; --lno);

	sp->lno = lno == 0 ? 1 : lno;
	sp->cno = 0;

	/* Redraw the screen, just in case. */
	F_SET(sp, S_REDRAW);
}

/*
 * Txt_indent and txt_outdent are truly strange.  ^T and ^D do movements to
 * the next or previous shiftwidth value, i.e. for a 1-based numbering, with
 * shiftwidth=3, ^T moves a cursor on the 7th, 8th or 9th column to the 10th
 * column, and ^D  moves it back.
 *
 * XXX
 * Technically, txt_indent, txt_outdent should part of the screen interface,
 * as they require knowledge of the size of a space character on the screen.
 * (Not the size of tabs, because tabs are logically composed of spaces.)
 * They're left in the text code  because they're complicated, not to mention
 * the gruesome awareness that if spaces aren't a single column on the screen
 * for any language we're into some serious, for lack of a better word,
 * "issues".
 */

/* Offset to next column of stop size. */
#define	STOP_OFF(c, stop)	(stop - (c) % stop)

/*
 * txt_indent --
 *	Handle ^T indents.
 */
static int
txt_indent(sp, tp)
	SCR *sp;
	TEXT *tp;
{
	u_long sw, ts;
	size_t cno, off, scno, spaces, tabs;

	sw = O_VAL(sp, O_SHIFTWIDTH);
	ts = O_VAL(sp, O_TABSTOP);

	/* Get the current screen column. */
	for (off = scno = 0; off < sp->cno; ++off)
		if (tp->lb[off] == '\t')
			scno += STOP_OFF(scno, ts);
		else
			++scno;

	/* Count up spaces/tabs needed to get to the target. */
	for (cno = scno, scno += STOP_OFF(scno, sw), tabs = 0;
	    cno + STOP_OFF(cno, ts) <= scno; ++tabs)
		cno += STOP_OFF(cno, ts);
	spaces = scno - cno;

	/* Make sure there's enough room. */
	BINC(sp, tp->lb, tp->lb_len, tp->len + spaces + tabs);

	/* Move the insert characters out of the way. */
	if (tp->insert)
		memmove(tp->lb + sp->cno + spaces + tabs,
		    tp->lb + sp->cno, tp->insert);

	/* Add new space/tab characters. */
	for (; tabs--; ++tp->len, ++tp->ai)
		tp->lb[sp->cno++] = '\t';
	for (; spaces--; ++tp->len, ++tp->ai)
		tp->lb[sp->cno++] = ' ';
	return (0);
}

/*
 * txt_outdent --
 *	Handle ^D outdents.
 *
 */
static int
txt_outdent(sp, tp)
	SCR *sp;
	TEXT *tp;
{
	u_long sw, ts;
	size_t cno, off, scno, spaces;

	sw = O_VAL(sp, O_SHIFTWIDTH);
	ts = O_VAL(sp, O_TABSTOP);

	/* Get the current screen column. */
	for (off = scno = 0; off < sp->cno; ++off)
		if (tp->lb[off] == '\t')
			scno += STOP_OFF(scno, ts);
		else
			++scno;

	/* Get the previous shiftwidth column. */
	for (cno = scno; --scno % sw != 0;);

	/* Decrement characters until less than or equal to that slot. */
	for (; cno > scno; --sp->cno, --tp->ai, ++tp->overwrite)
		if (tp->lb[--off] == '\t')
			cno -= STOP_OFF(cno, ts);
		else
			--cno;

	/* Spaces needed to get to the target. */
	spaces = scno - cno;

	/* Maybe just a delete. */
	if (spaces == 0)
		return (0);

	/* Make sure there's enough room. */
	BINC(sp, tp->lb, tp->lb_len, tp->len + spaces);

	/* Use up any overwrite characters. */
	for (; tp->overwrite && spaces; --spaces, ++tp->ai, --tp->overwrite)
		tp->lb[sp->cno++] = ' ';

	/* Maybe that was enough. */
	if (spaces == 0)
		return (0);

	/* Move the insert characters out of the way. */
	if (tp->insert)
		memmove(tp->lb + sp->cno + spaces,
		    tp->lb + sp->cno, tp->insert);

	/* Add new space characters. */
	for (; spaces--; ++tp->len, ++tp->ai)
		tp->lb[sp->cno++] = ' ';
	return (0);
}

/*
 * txt_resolve --
 *	Resolve the input text chain into the file.
 */
static int
txt_resolve(sp, ep, hp)
	SCR *sp;
	EXF *ep;
	HDR *hp;
{
	TEXT *tp;
	recno_t lno;

	tp = hp->next;

	/* The first line replaces a current line. */
	if (file_sline(sp, ep, tp->lno, tp->lb, tp->len))
		return (1);

	/* All subsequent lines are appended into the file. */
	for (lno = tp->lno; (tp = tp->next) != (TEXT *)&sp->txthdr; ++lno)
		if (file_aline(sp, ep, 0, lno, tp->lb, tp->len))
			return (1);
	return (0);
}
