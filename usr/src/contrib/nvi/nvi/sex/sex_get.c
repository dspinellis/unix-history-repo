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
static char sccsid[] = "@(#)sex_get.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <stdlib.h>
#include <ctype.h>

#include "vi.h"
#include "excmd.h"
#include "sex_screen.h"

static void	repaint __P((SCR *, int, char *, size_t));

#define	DISPLAY(wval, ch, col) {					\
	size_t __len;							\
	int __ch;							\
	if ((__ch = (ch)) == '\t') {					\
		__len = O_VAL(sp, O_TABSTOP) -				\
		    ((col) % O_VAL(sp, O_TABSTOP));			\
		(col) += (wval) = __len;				\
		while (__len--)						\
			putc(' ', (sp)->stdfp);				\
	} else {							\
		(col) += (wval) = cname[(__ch)].len;			\
		(void)fprintf((sp)->stdfp,				\
		    "%.*s", cname[(__ch)].len, cname[(__ch)].name);	\
	}								\
}

#define	ERASECH {							\
	for (cnt = tp->wd[tp->len]; cnt > 0; --cnt, --col)		\
		(void)fprintf(sp->stdfp, "%s", "\b \b");		\
}

/*
 * sex_get --
 *	Fill a buffer from the terminal for ex.
 */
int
sex_get(sp, ep, hp, prompt, flags)
	SCR *sp;
	EXF *ep;
	HDR *hp;
	int prompt;
	u_int flags;
{
				/* State of quotation. */
	enum { Q_NOTSET, Q_THISCHAR } quoted;
	CHNAME *cname;		/* Character map. */
	TEXT *tp;		/* Input text structures. */
	int ch;			/* Input character. */
	size_t col;		/* 0-N: screen column. */
	size_t cnt;

#ifdef DEBUG
	if (LF_ISSET(~TXT_VALID_EX) || !LF_ISSET(TXT_CR))
		abort();
#endif
	/*
	 * Get one TEXT structure with some initial buffer space, reusing
	 * the last one if it's big enough.  (All TEXT bookkeeping fields
	 * default to 0 -- text_init() handles this.)
	 */
	if (hp->next != hp) {
		tp = hp->next;
		if (tp->next != (TEXT *)hp || tp->lb_len < 32) {
			hdr_text_free(hp);
			goto newtp;
		}
		tp->len = 0;
	} else {
newtp:		if ((tp = text_init(sp, NULL, 0, 32)) == NULL)
			return (1);
		HDR_INSERT(tp, hp, next, prev, TEXT);
	}

	cname = sp->cname;
	if (LF_ISSET(TXT_PROMPT) && O_ISSET(sp, O_PROMPT)) {
		(void)fprintf(sp->stdfp, "%s", cname[prompt].name);
		col = cname[prompt].len;
	} else
		col = 0;

	for (quoted = Q_NOTSET;;) {
		(void)fflush(sp->stdfp);

		ch = term_key(sp, flags & TXT_GETKEY_MASK);

		BINC(sp, tp->lb, tp->lb_len, tp->len + 1);
		BINC(sp, tp->wd, tp->wd_len, tp->len + 1);

		if (quoted == Q_THISCHAR) {
			ERASECH;
			goto ins_ch;
		}

		switch (sp->special[ch]) {
		case K_CNTRLZ:
			sex_suspend(sp);
			/* FALLTHROUGH */
		case K_CNTRLR:
			repaint(sp, prompt, tp->lb, tp->len);
			break;
		case K_CR:
		case K_NL:
			if (LF_ISSET(TXT_NLECHO)) {
				(void)putc('\n', sp->stdfp);
				(void)fflush(sp->stdfp);
			}
			/* Terminate with a newline, needed by filter. */
			tp->lb[tp->len] = '\0';
			return (0);
		case K_VERASE:
			if (tp->len) {
				--tp->len;
				ERASECH;
			}
			break;
		case K_VKILL:
			for (; tp->len; --tp->len)
				ERASECH;
			break;
		case K_VLNEXT:
			(void)fprintf(stderr, "%s%c", cname['^'].name, '\b');
			quoted = Q_THISCHAR;
			break;
		case K_VWERASE:
			/* Move to the last non-space character. */
			while (tp->len)
				if (!isspace(tp->lb[--tp->len])) {
					++tp->len;
					break;
				} else
					ERASECH;

			/* Move to the last space character. */
			while (tp->len)
				if (isspace(tp->lb[--tp->len])) {
					++tp->len;
					break;
				} else
					ERASECH;
			break;
		default:
ins_ch:			tp->lb[tp->len] = ch;
			DISPLAY(tp->wd[tp->len], ch, col);
			++tp->len;
			quoted = Q_NOTSET;
			break;
		}
	}
	/* NOTREACHED */
}

/*
 * repaint --
 *	Repaint the line.
 */
static void
repaint(sp, prompt, p, len)
	SCR *sp;
	int prompt;
	char *p;
	size_t len;
{
	CHNAME *cname;
	size_t col;
	u_char width;

	cname = sp->cname;

	(void)putc('\n', sp->stdfp);
	if (prompt && O_ISSET(sp, O_PROMPT)) {	/* Display prompt. */
		(void)fprintf(sp->stdfp, "%s", cname[prompt].name);
		col = cname[prompt].len;
	} else
		col = 0;

	while (len--)
		DISPLAY(width, *p++, col);
}
