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
static char sccsid[] = "@(#)ex_undo.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_undol -- U
 *	Undo changes to this line, or roll forward.
 */
int
ex_undol(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	MARK m;

	if (O_ISSET(sp, O_NUNDO)) {
		if (log_forward(sp, ep, &m))
			return (1);
	} else {
		if (log_setline(sp, ep, &m))
			return (1);
	}

	sp->lno = m.lno;
	sp->cno = m.cno;

	F_SET(sp, S_AUTOPRINT);

	return (0);
}

/*
 * ex_undo -- u
 *	Undo the last change.
 */
int
ex_undo(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	MARK m;

	if (O_ISSET(sp, O_NUNDO)) {
		if (log_backward(sp, ep, &m))
			return (1);
	} else {
		if (!F_ISSET(ep, F_UNDO)) {
			ep->lundo = UFORWARD;
			F_SET(ep, F_UNDO);
		}

		switch (ep->lundo) {
		case UBACKWARD:
			if (log_forward(sp, ep, &m)) {
				F_CLR(ep, F_UNDO);
				return (1);
			}
			ep->lundo = UFORWARD;
			break;
		case UFORWARD:
			if (log_backward(sp, ep, &m)) {
				F_CLR(ep, F_UNDO);
				return (1);
			}
			ep->lundo = UBACKWARD;
			break;
		}
	}
	sp->lno = m.lno;
	sp->cno = m.cno;

	F_SET(sp, S_AUTOPRINT);

	return (0);
}
