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
static char sccsid[] = "@(#)v_window.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "vcmd.h"

/*
 * v_window --
 *	Switch windows.
 */
int
v_window(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	SCR *p;

	*rp = *fm;

	/*
	 * Try for the next lower window, or, go back to the first
	 * window on the stack.
	 */
	if (sp->child != NULL)
		sp->snext = sp->child;
	else if (sp->parent == NULL) {
		msgq(sp, M_ERR, "No other window to switch to");
		return (1);
	} else {
		for (p = sp; p->parent != NULL; p = p->parent);
		sp->snext = p;
	}

	/*
	 * Display the status line so the user can find the window
	 * they want.
	 */
	(void)status(sp, ep, fm->lno, 0);
	(void)sp->s_refresh(sp, ep);
	F_SET(sp, S_SSWITCH);
	return (0);
}
