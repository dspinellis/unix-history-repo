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
static char sccsid[] = "@(#)v_increment.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"

static char * const fmt[] = {
#define	DEC	0
	"%.*ld",
#define	SDEC	1
	"%+.*ld",
#define	HEXC	2
	"%#0.*lX",
#define	HEXL	3
	"%#0.*lx",
#define	OCTAL	4
	"%#0.*lo",
};

/*
 * v_increment -- [count]#[#+-]
 *	Increment/decrement a keyword number.
 */
int
v_increment(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	u_long ulval;
	long lval;
	size_t len, nlen;
	int rval;
	char *ntype, *np, *p, nbuf[60];

	/* Do repeat operations. */
	if (vp->character == '#')
		vp->character = sp->inc_lastch;

	/* Get new value. */
	if (F_ISSET(vp, VC_C1SET))
		sp->inc_lastval = vp->count;

	if (vp->character != '+' && vp->character != '-') {
		msgq(sp, M_ERR, "usage: %s.", vp->kp->usage);
		return (1);
	}
	sp->inc_lastch = vp->character;

	/* Figure out the resulting type and number. */
	p = vp->keyword;
	len = vp->klen;
	if (len > 1 && p[0] == '0') {
		if (vp->character == '+') {
			ulval = strtoul(vp->keyword, NULL, 0);
			if (ULONG_MAX - ulval < sp->inc_lastval)
				goto overflow;
			ulval += sp->inc_lastval;
		} else {
			ulval = strtoul(vp->keyword, NULL, 0);
			if (ulval < sp->inc_lastval)
				goto underflow;
			ulval -= sp->inc_lastval;
		}
		ntype = fmt[OCTAL];
		if (len > 2)
			if (p[1] == 'X')
				ntype = fmt[HEXC];
			else if (p[1] == 'x')
				ntype = fmt[HEXL];
		nlen = snprintf(nbuf, sizeof(nbuf), ntype, len, ulval);
	} else {
		if (vp->character == '+') {
			lval = strtol(vp->keyword, NULL, 0);
			if (LONG_MAX - lval < sp->inc_lastval) {
overflow:			msgq(sp, M_ERR,
				    "Resulting number too large.");
				return (1);
			}
			lval += sp->inc_lastval;
		} else {
			lval = strtol(vp->keyword, NULL, 0);
			if (lval < 0 && -(LONG_MIN - lval) > sp->inc_lastval) {
underflow:			msgq(sp, M_ERR,
				    "Resulting number too small.");
				return (1);
			}
			lval -= sp->inc_lastval;
		}
		ntype = *vp->keyword == '+' || *vp->keyword == '-' ?
		    fmt[SDEC] : fmt[DEC];
		nlen = snprintf(nbuf, sizeof(nbuf), ntype, len, lval);
	}

	if ((p = file_gline(sp, ep, fm->lno, &len)) == NULL) {
		GETLINE_ERR(sp, fm->lno);
		return (1);
	}

	if ((np = malloc(len + nlen)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	memmove(np, p, fm->cno);
	memmove(np + fm->cno, nbuf, nlen);
	memmove(np + fm->cno + nlen,
	    p + fm->cno + vp->klen, len - fm->cno - vp->klen);
	p = np;
	len = len - vp->klen + nlen;

	if (file_sline(sp, ep, fm->lno, p, len))
		rval = 1;
	else {
		*rp = *fm;
		rval = 0;
	}
	if (np)
		free(np);
	return (rval);
}
