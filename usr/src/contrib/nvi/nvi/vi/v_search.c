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
static char sccsid[] = "@(#)v_search.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"

static int getptrn __P((SCR *, EXF *, int, char **));

/*
 * v_searchn -- n
 *	Repeat last search.
 */
int
v_searchn(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	switch (sp->searchdir) {
	case BACKWARD:
		if (b_search(sp, ep, fm, rp, NULL, NULL,
		    SEARCH_MSG | SEARCH_PARSE | SEARCH_TERM))
			return (1);
		break;
	case FORWARD:
		if (f_search(sp, ep, fm, rp, NULL, NULL,
		    SEARCH_MSG | SEARCH_PARSE | SEARCH_TERM))
			return (1);
		break;
	case NOTSET:
		msgq(sp, M_ERR, "No previous search pattern.");
		return (1);
	default:
		abort();
	}
	return (0);
}

/*
 * v_searchN -- N
 *	Reverse last search.
 */
int
v_searchN(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	switch (sp->searchdir) {
	case BACKWARD:
		if (f_search(sp, ep, fm, rp, NULL, NULL,
		    SEARCH_MSG | SEARCH_PARSE | SEARCH_TERM))
			return (1);
		break;
	case FORWARD:
		if (b_search(sp, ep, fm, rp, NULL, NULL,
		    SEARCH_MSG | SEARCH_PARSE | SEARCH_TERM))
			return (1);
		break;
	case NOTSET:
		msgq(sp, M_ERR, "No previous search pattern.");
		return (1);
	default:
		abort();
	}
	return (0);
}

/*
 * v_searchw -- [count]^A
 *	Search for the word under the cursor.
 */
int
v_searchw(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	size_t blen, len;
	int rval;
	char *bp;

	len = vp->kbuflen + sizeof(RE_NOTINWORD) * 2;
	GET_SPACE(sp, bp, blen, len);
	(void)snprintf(bp, blen,
	    "%s%s%s", RE_NOTINWORD, vp->keyword, RE_NOTINWORD);
		
	rval = f_search(sp, ep, fm, rp, bp, NULL, SEARCH_MSG | SEARCH_TERM);

	FREE_SPACE(sp, bp, blen);

	++rp->cno;			/* Offset by one. */
	return (rval);
}

/*
 * v_searchb -- [count]?RE?offset
 *	Search backward.
 */
int
v_searchb(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	char *ptrn;

	if (getptrn(sp, ep, '?', &ptrn))
		return (1);
	if (ptrn == NULL) {
		*rp = *fm;
		return (0);
	}
	if (b_search(sp, ep, fm, rp, ptrn, NULL,
	    SEARCH_MSG | SEARCH_PARSE | SEARCH_SET))
		return (1);
	return (0);
}

/*
 * v_searchf -- [count]/RE/offset
 *	Search forward.
 */
int
v_searchf(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	char *ptrn;

	if (getptrn(sp, ep, '/', &ptrn))
		return (1);
	if (ptrn == NULL) {
		*rp = *fm;
		return (0);
	}
	if (f_search(sp, ep, fm, rp, ptrn, NULL,
	    SEARCH_MSG | SEARCH_PARSE | SEARCH_SET))
		return (1);
	return (0);
}

/*
 * getptrn --
 *	Get the search pattern.
 */
static int
getptrn(sp, ep, prompt, storep)
	SCR *sp;
	EXF *ep;
	int prompt;
	char **storep;
{
	TEXT *tp;

	if (sp->s_get(sp, ep, &sp->bhdr, prompt,
	    TXT_BS | TXT_CR | TXT_ESCAPE | TXT_PROMPT))
		return (1);

	tp = sp->bhdr.next;
	if (tp->len == 1)
		*storep = NULL;
	else
		*storep = tp->lb;
	return (0);
}
