/*-
 * Copyright (c) 1992, 1993
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
static char sccsid[] = "@(#)cut.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"

static int	cutline __P((SCR *, EXF *, recno_t, size_t, size_t, TEXT **));

/* 
 * cut --
 *	Put a range of lines/columns into a malloc'd buffer for later use.
 */
int
cut(sp, ep, buffer, fm, tm, lmode)
	SCR *sp;
	EXF *ep;
	int buffer, lmode;
	MARK *fm, *tm;
{
	CB *a, *cb;
	TEXT *tp;
	size_t len;
	recno_t lno;
	int append, indx;

	append = isupper(buffer);
	CBNAME(sp, buffer, cb);

	/*
	 * Upper-case buffer names map into lower-case buffers, but with
	 * append mode set so the buffer is appended to, not overwritten.
	 * Also note if the buffer has changed modes.
	 */
	if (append && !lmode && cb->flags & CB_LMODE)
		msgq(sp, M_INFO, "Buffer %s changed to line mode",
		    charname(sp, buffer));

	/*
	 * If changing the default buffer, buffers #1 through #8 get
	 * rotated up one, the current default buffer gets placed in
	 * slot #1, and slot #9 drops off the end.
	 */
	if (buffer == DEFCB) {
		a = &sp->cuts['9'];
		if (a->txthdr.next != NULL && a->txthdr.next != &a->txthdr)
			hdr_text_free(&a->txthdr);
		for (indx = '9'; indx > '1'; --indx) {
			a = &sp->cuts[indx - 1];
			if (a->txthdr.next == NULL ||
			    a->txthdr.next == &a->txthdr) {
				HDR_INIT(sp->cuts[indx].txthdr, next, prev);
			} else {
				sp->cuts[indx] = *a;
				((TEXT *)a->txthdr.next)->prev =
				    ((TEXT *)a->txthdr.prev)->next = 
				    (TEXT *)&sp->cuts[indx].txthdr;
			}
		}
		a = &sp->cuts[DEFCB];
		if (a->txthdr.next == NULL ||
		    a->txthdr.next == &a->txthdr) {
			HDR_INIT(sp->cuts['1'].txthdr, next, prev);
		} else {
			sp->cuts['1'] = *a;
			((TEXT *)a->txthdr.next)->prev =
			    ((TEXT *)a->txthdr.prev)->next =
			    (TEXT *)&sp->cuts['1'].txthdr;
		}
		HDR_INIT(cb->txthdr, next, prev);
	}

	/* Initialize buffer. */
	if (cb->txthdr.next == NULL) {
		HDR_INIT(cb->txthdr, next, prev);
	} else if (!append) {
		hdr_text_free(&cb->txthdr);
		cb->len = 0;
		cb->flags = 0;
	}

#if DEBUG && 0
	TRACE(sp, "cut: from {%lu, %d}, to {%lu, %d}%s\n",
	    fm->lno, fm->cno, tm->lno, tm->cno, lmode ? " LINE MODE" : "");
#endif
	if (lmode) {
		for (lno = fm->lno; lno <= tm->lno; ++lno) {
			if (cutline(sp, ep, lno, 0, 0, &tp))
				goto mem;
			HDR_INSERT(tp, &cb->txthdr, next, prev, TEXT);
			cb->len += tp->len;
		}
		cb->flags |= CB_LMODE;
		return (0);
	}
		
	/* Get the first line. */
	len = fm->lno < tm->lno ? 0 : tm->cno - fm->cno;
	if (cutline(sp, ep, fm->lno, fm->cno, len, &tp))
		goto mem;

	HDR_INSERT(tp, &cb->txthdr, next, prev, TEXT);
	cb->len += tp->len;

	for (lno = fm->lno; ++lno < tm->lno;) {
		if (cutline(sp, ep, lno, 0, 0, &tp))
			goto mem;
		HDR_INSERT(tp, &cb->txthdr, next, prev, TEXT);
		cb->len += tp->len;
	}

	if (tm->lno > fm->lno && tm->cno > 0) {
		if (cutline(sp, ep, lno, 0, tm->cno, &tp)) {
mem:			if (append)
				msgq(sp, M_ERR,
				    "Contents of %s buffer lost.",
				    charname(sp, buffer));
			hdr_text_free(&cb->txthdr);
			cb->len = 0;
			cb->flags = 0;
			return (1);
		}
		HDR_INSERT(tp, &cb->txthdr, next, prev, TEXT);
		cb->len += tp->len;
	}
	return (0);
}

/*
 * cutline --
 *	Cut a portion of a single line.  A length of zero means to cut
 * 	from the MARK to the end of the line.
 */
static int
cutline(sp, ep, lno, fcno, len, newp)
	SCR *sp;
	EXF *ep;
	recno_t lno;
	size_t fcno, len;
	TEXT **newp;
{
	TEXT *tp;
	size_t llen;
	char *p;

	if ((p = file_gline(sp, ep, lno, &llen)) == NULL) {
		GETLINE_ERR(sp, lno);
		return (1);
	}

	if ((*newp = tp = text_init(sp, NULL, 0, llen)) == NULL)
		return (1);

	if (llen == 0) {
#if DEBUG && 0
		TRACE(sp, "{}\n");
#endif
	} else {
		if (len == 0)
			len = llen - fcno;
		memmove(tp->lb, p + fcno, len);
		tp->len = len;
#if DEBUG && 0
		TRACE(sp, "\t{%.*s}\n", MIN(len, 20), p + fcno);
#endif
	}
	return (0);
}

/*
 * put --
 *	Put text buffer contents into the file.
 */	
int
put(sp, ep, buffer, cp, rp, append)
	SCR *sp;
	EXF *ep;
	int buffer, append;
	MARK *cp, *rp;
{
	CB *cb;
	TEXT *ltp, *tp;
	recno_t lno;
	size_t blen, clen, len;
	int lmode;
	char *bp, *p, *t;

	CBNAME(sp, buffer, cb);
	CBEMPTY(sp, buffer, cb);

	tp = cb->txthdr.next;
	lmode = cb->flags & CB_LMODE;

	/*
	 * If buffer was created in line mode, append each new line into the
	 * file.  Historical practice is that the cursor ends up on the first
	 * non-blank character of the first line inserted.
	 */
	if (lmode) {
		if (append) {
			for (lno = cp->lno;
			    tp != (TEXT *)&cb->txthdr; ++lno, tp = tp->next)
				if (file_aline(sp, ep, 1, lno, tp->lb, tp->len))
					return (1);
			rp->lno = cp->lno + 1;
		} else if ((lno = cp->lno) != 1) {
			for (--lno;
			    tp != (TEXT *)&cb->txthdr; tp = tp->next, ++lno)
				if (file_aline(sp, ep, 1, lno, tp->lb, tp->len))
					return (1);
			rp->lno = cp->lno;
		} else {
			if (file_iline(sp, ep, (recno_t)1, tp->lb, tp->len))
				return (1);
			for (lno = 1;
			    (tp = tp->next) != (TEXT *)&cb->txthdr; ++lno)
				if (file_aline(sp, ep, 1, lno, tp->lb, tp->len))
					return (1);
			rp->lno = 1;
		}
		if (nonblank(sp, ep, rp->lno, &rp->cno))
			rp->cno = 0;
	}
	/*
	 * If buffer was cut in character mode, replace the current line with
	 * one built from the portion of the first line to the left of the
	 * split plus the first line in the CB.  Append each intermediate line
	 * in the CB.  Append a line built from the portion of the first line
	 * to the right of the split plus the last line in the CB.
	 */
	else {
		/* Get the first line. */
		lno = cp->lno;
		if ((p = file_gline(sp, ep, lno, &len)) == NULL) {
			GETLINE_ERR(sp, lno);
			return (1);
		}

		GET_SPACE(sp, bp, blen, tp->len + len + 1);
		t = bp;

		/* Original line, left of the split. */
		if (len > 0 && (clen = cp->cno + (append ? 1 : 0)) > 0) {
			memmove(bp, p, clen);
			p += clen;
			t += clen;
		}

		/* First line from the CB. */
		memmove(t, tp->lb, tp->len);
		t += tp->len;

		/* Calculate length left in original line. */
		clen = len ? len - cp->cno - (append ? 1 : 0) : 0;

		/*
		 * If no more lines in the CB, append the rest of the original
		 * line and quit.  Otherwise, build the last line before doing
		 * the intermediate lines, because the line changes will lose
		 * the cached line.
		 */
		if (tp->next == (TEXT *)&cb->txthdr) {
			/*
			 * Historical practice is that if a non-line mode put
			 * is inside a single line, the cursor ends up on the
			 * last character inserted.
			 */
			rp->lno = lno;
			rp->cno = (t - bp) - 1;

			if (clen > 0) {
				memmove(t, p, clen);
				t += clen;
			}
			if (file_sline(sp, ep, lno, bp, t - bp))
				goto mem;
		} else {
			/*
			 * Have to build both the first and last lines of the
			 * put before doing any sets or we'll lose the cached
			 * line.  Build both the first and last lines in the
			 * same buffer, so we don't have to have another buffer
			 * floating around.
			 *
			 * Last part of original line; check for space, reset
			 * the pointer into the buffer.
			 */
			ltp = cb->txthdr.prev;
			len = t - bp;
			ADD_SPACE(sp, bp, blen, ltp->len + clen);
			t = bp + len;

			/* Add in last part of the CB. */
			memmove(t, ltp->lb, ltp->len);
			if (clen)
				memmove(t + ltp->len, p, clen);
			clen += ltp->len;

			/*
			 * Now: bp points to the first character of the first
			 * line, t points to the last character of the last
			 * line, t - bp is the length of the first line, and
			 * clen is the length of the last.  Just figured you'd
			 * want to know.
			 *
			 * Output the line replacing the original line.
			 */
			if (file_sline(sp, ep, lno, bp, t - bp))
				goto mem;

			/*
			 * Historical practice is that if a non-line mode put
			 * covers multiple lines, the cursor ends up on the
			 * first character inserted.  (Of course.)
			 */
			rp->lno = lno;
			rp->cno = (t - bp) - 1;

			/* Output any intermediate lines in the CB. */
			for (tp = tp->next; tp->next != (TEXT *)&cb->txthdr;
			    ++lno, tp = tp->next)
				if (file_aline(sp, ep, 1, lno, tp->lb, tp->len))
					goto mem;

			if (file_aline(sp, ep, 1, lno, t, clen)) {
mem:				FREE_SPACE(sp, bp, blen);
				return (1);
			}
		}
		FREE_SPACE(sp, bp, blen);
	}

	/* Shift any marks in the range. */
	mark_insert(sp, ep, cp, rp);

	/* Reporting... */
	sp->rptlines[L_PUT] += lno - cp->lno;

	return (0);
}

/*
 * text_init --
 *	Allocate a new TEXT structure.
 */
TEXT *
text_init(sp, p, len, total_len)
	SCR *sp;
	char *p;
	size_t len, total_len;
{
	TEXT *tp;

	if ((tp = malloc(sizeof(TEXT))) == NULL)
		goto mem;
	if ((tp->lb = malloc(tp->lb_len = total_len)) == NULL) {
		free(tp);
mem:		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (NULL);
	}
#ifdef DEBUG
	if (total_len)
		memset(tp->lb, 0, total_len - 1);
#endif

	if (p != NULL && len != 0)
		memmove(tp->lb, p, len);
	tp->len = len;
	tp->ai = tp->insert = tp->offset = tp->overwrite = 0;
	tp->wd = NULL;
	tp->wd_len = 0;

	return (tp);
}

/*
 * hdr_text_free --
 *	Free a chain of text structures.
 */
void
hdr_text_free(hp)
	HDR *hp;
{
	TEXT *tp;

	while (hp->next != hp) {
		tp = hp->next;
		HDR_DELETE(tp, next, prev, TEXT);
		text_free(tp);
	}
}

/*
 * text_free --
 *	Free a text structure.
 */
void
text_free(tp)
	TEXT *tp;
{
	if (tp->lb != NULL)
		FREE(tp->lb, tp->lb_len);
	if (tp->wd != NULL)
		FREE(tp->wd, tp->wd_len);
	FREE(tp, sizeof(TEXT));
}
