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
static char sccsid[] = "@(#)ex_append.c	8.15 (Berkeley) 4/11/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
#include <sys/time.h>

#include <bitstring.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "excmd.h"

enum which {APPEND, CHANGE, INSERT};

static int aci __P((SCR *, EXF *, EXCMDARG *, enum which));

/*
 * ex_append -- :[line] a[ppend][!]
 *	Append one or more lines of new text after the specified line,
 *	or the current line if no address is specified.
 */
int
ex_append(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (aci(sp, ep, cmdp, APPEND));
}

/*
 * ex_change -- :[line[,line]] c[hange][!] [count]
 *	Change one or more lines to the input text.
 */
int
ex_change(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (aci(sp, ep, cmdp, CHANGE));
}

/*
 * ex_insert -- :[line] i[nsert][!]
 *	Insert one or more lines of new text before the specified line,
 *	or the current line if no address is specified.
 */
int
ex_insert(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (aci(sp, ep, cmdp, INSERT));
}

static int
aci(sp, ep, cmdp, cmd)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
	enum which cmd;
{
	MARK m;
	TEXTH *sv_tiqp, tiq;
	TEXT *tp;
	recno_t cnt;
	u_int flags;
	int rval;

	rval = 0;

	/*
	 * Set input flags; the ! flag turns off autoindent for append,
	 * change and insert.
	 */
	LF_INIT(TXT_DOTTERM | TXT_NLECHO);
	if (!F_ISSET(cmdp, E_FORCE) && O_ISSET(sp, O_AUTOINDENT))
		LF_SET(TXT_AUTOINDENT);
	if (O_ISSET(sp, O_BEAUTIFY))
		LF_SET(TXT_BEAUTIFY);

	/* Input is interruptible. */
	F_SET(sp, S_INTERRUPTIBLE);

	/*
	 * If this code is called by vi, the screen TEXTH structure (sp->tiqp)
	 * may already be in use, e.g. ":append|s/abc/ABC/" would fail as we're
	 * only halfway through the line when the append code fires.  Use the
	 * local structure instead.
	 */
	if (IN_VI_MODE(sp)) {
		memset(&tiq, 0, sizeof(TEXTH));
		CIRCLEQ_INIT(&tiq);
		sv_tiqp = sp->tiqp;
		sp->tiqp = &tiq;
	}

	switch (sp->s_get(sp, ep, sp->tiqp, 0, flags)) {
	case INP_OK:
		break;
	case INP_EOF:
	case INP_ERR:
		goto err;
	}
	
	/*
	 * If doing a change, replace lines for as long as possible.
	 * Then, append more lines or delete remaining lines.  Inserts
	 * are the same as appends to the previous line.
	 */
	m = cmdp->addr1;
	if (cmd == INSERT) {
		--m.lno;
		cmd = APPEND;
	}

	tp = sp->tiqp->cqh_first;
	if (cmd == CHANGE)
		for (;; tp = tp->q.cqe_next) {
			if (m.lno > cmdp->addr2.lno) {
				cmd = APPEND;
				--m.lno;
				break;
			}
			if (tp == (TEXT *)sp->tiqp) {
				for (cnt =
				    (cmdp->addr2.lno - m.lno) + 1; cnt--;)
					if (file_dline(sp, ep, m.lno))
						goto err;
				goto done;
			}
			if (file_sline(sp, ep, m.lno, tp->lb, tp->len))
				goto err;
			sp->lno = m.lno++;
		}

	if (cmd == APPEND)
		for (; tp != (TEXT *)sp->tiqp; tp = tp->q.cqe_next) {
			if (file_aline(sp, ep, 1, m.lno, tp->lb, tp->len)) {
err:				rval = 1;
				goto done;
			}
			sp->lno = ++m.lno;
		}

done:	if (IN_VI_MODE(sp)) {
		sp->tiqp = sv_tiqp;
		text_lfree(&tiq);
	}
	return (rval);
}
