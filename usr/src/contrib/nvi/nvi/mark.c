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
static char sccsid[] = "@(#)mark.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <string.h>

#include "vi.h"

/*
 * Historic vi got mark updates wrong.  Marks were fixed, and if the line
 * subsequently changed modifications of the line wouldn't update the mark's
 * offset in the line.  This is arguably correct in some cases, e.g. when
 * the user wants to keep track of the start of a line, even after inserting
 * text at the beginning of the line.  However, given that single quotes mark
 * lines, not specific locations in the line, and that it would be difficult
 * to reproduce the exact vi semantics, these routines do it "correctly".
 *
 * XXX
 * Right now, it's expensive to find the marks since we traverse the array
 * linearly.  Should have a doubly linked list of mark entries so we can
 * traverse it quickly on updates.
 *
 * In the historic vi, marks would return if the operation was undone.  This
 * code doesn't handle that problem.  It should be done as part of TXN undo,
 * logged from here.
 */

/*
 * mark_init --
 *	Initialize the marks for a file.
 */
int
mark_init(sp, ep)
	SCR *sp;
	EXF *ep;
{
	MARK m;

	/* Default absolute marks. */
	ep->absmark.lno = m.lno = 1;
	ep->absmark.cno = m.cno = 0;
	return (mark_set(sp, ep, ABSMARK1, &m));
}

/*
 * mark_set --
 *	Set the location referenced by a mark.  Note, setting either of
 *	the absolute mark locations sets both, so that "m'" and "m`" work
 *	like they, ah, for lack of a better word, should.
 */
int
mark_set(sp, ep, key, mp)
	SCR *sp;
	EXF *ep;
	int key;
	MARK *mp;
{
	if (key > UCHAR_MAX) {
		msgq(sp, M_BERR, "Invalid mark name.");
		return (1);
	}
	if (key == ABSMARK1 || key == ABSMARK2) {
		ep->absmark = ep->marks[ABSMARK1];
		ep->marks[ABSMARK1] = ep->marks[ABSMARK2] = *mp;
	} else
		ep->marks[key] = *mp;
	return (0);
}

/*
 * mark_get --
 *	Get the location referenced by a mark.
 */
MARK *
mark_get(sp, ep, key)
	SCR *sp;
	EXF *ep;
	int key;
{
	MARK *mp;

	if (key > UCHAR_MAX) {
		msgq(sp, M_BERR, "Invalid mark name.");
		return (NULL);
	}
	mp = &ep->marks[key];
	if (mp->lno == OOBLNO) {
		msgq(sp, M_BERR, "Mark '%s not set.", sp->cname[key].name);
                return (NULL);
	}
	return (mp);
}

/*
 * mark_delete --
 *	Update the marks based on a deletion.
 */
void
mark_delete(sp, ep, fm, tm, lmode)
	SCR *sp;
	EXF *ep;
	MARK *fm, *tm;
	int lmode;
{
	register MARK *mp;
	register int cno, cnt, lno;
	
	cno = tm->cno - fm->cno;
	if (tm->lno == fm->lno) {
		lno = fm->lno;
		for (cnt = 0, mp = ep->marks;
		    cnt < sizeof(ep->marks) / sizeof(ep->marks[0]);
		    ++cnt, ++mp) {
			if (mp->lno != lno || mp->cno < fm->cno)
				continue;
			if (lmode || mp->cno < tm->cno)
				mp->lno = OOBLNO;
			else
				mp->cno -= cno;
		}
	} else {
		lno = tm->lno - fm->lno + 1;
		for (cnt = 0, mp = ep->marks;
		    cnt < sizeof(ep->marks) / sizeof(ep->marks[0]);
		    ++cnt, ++mp) {
			if (mp->lno < fm->lno)
				continue;
			if (mp->lno == fm->lno)
				if (lmode || mp->cno >= fm->cno)
					mp->lno = OOBLNO;
				else
					mp->cno -= cno;
			else
				mp->lno -= lno;
		}
	}
}

/*
 * mark_insert --
 *	Update the marks based on an insertion.
 */
void
mark_insert(sp, ep, fm, tm)
	SCR *sp;
	EXF *ep;
	MARK *fm, *tm;
{
	register MARK *mp;
	register int cno, cnt, lno;
	
	lno = tm->lno - fm->lno;
	cno = tm->cno - fm->cno;
	for (cnt = 0, mp = ep->marks;
	    cnt < sizeof(ep->marks) / sizeof(ep->marks[0]); ++cnt, ++mp) {
		if (mp->lno < fm->lno)
			continue;
		if (mp->lno > tm->lno) {
			mp->lno += lno;
			continue;
		}
		if (mp->cno < fm->cno)
			continue;

		mp->lno += lno;
		mp->cno += cno;
	}
}
