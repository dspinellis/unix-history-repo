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
static char sccsid[] = "@(#)ex_bdisplay.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>
#include <ctype.h>

#include "vi.h"
#include "excmd.h"

static void db __P((SCR *, char *, CB *));

/*
 * ex_bdisplay -- :bdisplay
 *	Display cut buffer contents.
 */
int
ex_bdisplay(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	CB *cb;
	int cnt, displayed;

	displayed = 0;
	for (cb = sp->cuts, cnt = 0; cnt < UCHAR_MAX; ++cb, ++cnt)
		if (cb->txthdr.next != NULL && cb->txthdr.next != &cb->txthdr) {
			displayed = 1;
			db(sp, charname(sp, cnt), cb);
		}
	if (sp->cuts[DEFCB].txthdr.next != NULL &&
	    sp->cuts[DEFCB].txthdr.next != &sp->cuts[DEFCB].txthdr) {
		displayed = 1;
		db(sp, "default buffer", &sp->cuts[DEFCB]);
	}
	if (!displayed)
		msgq(sp, M_VINFO, "No buffers to display.");
	return (0);
}

static void
db(sp, name, cb)
	SCR *sp;
	char *name;
	CB *cb;
{
	TEXT *tp;

	(void)fprintf(sp->stdfp, "================ %s%s\n", name,
	    F_ISSET(cb, CB_LMODE) ? " (line mode)" : "");
	for (tp = cb->txthdr.next; tp != (TEXT *)&cb->txthdr; tp = tp->next)
		(void)fprintf(sp->stdfp, "%.*s\n", (int)tp->len, tp->lb);
}
