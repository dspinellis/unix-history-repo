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
static char sccsid[] = "@(#)ex_append.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "excmd.h"

enum which {APPEND, CHANGE};

static int ac __P((SCR *, EXF *, EXCMDARG *, enum which));

/*
 * ex_append -- :address append[!]
 *	Append one or more lines of new text after the specified line,
 *	or the current line if no address is specified.
 */
int
ex_append(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (ac(sp, ep, cmdp, APPEND));
}

/*
 * ex_change -- :range change[!] [count]
 *	Change one or more lines to the input text.
 */
int
ex_change(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (ac(sp, ep, cmdp, CHANGE));
}

static int
ac(sp, ep, cmdp, cmd)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
	enum which cmd;
{
	MARK m;
	TEXT *tp;
	recno_t cnt;
	int rval, set;

	/* The ! flag turns off autoindent for change and append. */
	if (F_ISSET(cmdp, E_FORCE)) {
		set = O_ISSET(sp, O_AUTOINDENT);
		O_CLR(sp, O_AUTOINDENT);
	} else
		set = 0;

	rval = 0;

	/*
	 * If doing a change, replace lines as long as possible.
	 * Then, append more lines, or delete remaining lines.
	 */
	m = cmdp->addr1;
	if (m.lno == 0)
		cmd = APPEND;
	if (cmd == CHANGE)
		for (;; ++m.lno) {
			if (m.lno > cmdp->addr2.lno) {
				cmd = APPEND;
				--m.lno;
				break;
			}
			if (sp->s_get(sp, ep,
			    &sp->bhdr, 0, TXT_BEAUTIFY | TXT_CR | TXT_NLECHO)) {
				rval = 1;
				goto done;
			}
			tp = sp->bhdr.next;
			if (tp->len == 1 && tp->lb[0] == '.') {
				cnt = cmdp->addr2.lno - m.lno;
				while (cnt--)
					if (file_dline(sp, ep, m.lno)) {
						rval = 1;
						goto done;
					}
				goto done;
			}
			if (file_sline(sp, ep, m.lno, tp->lb, tp->len)) {
				rval = 1;
				goto done;
			}
		}

	if (cmd == APPEND)
		for (;; ++m.lno) {
			if (sp->s_get(sp, ep,
			    &sp->bhdr, 0, TXT_BEAUTIFY | TXT_CR | TXT_NLECHO)) {
				rval = 1;
				goto done;
			}
			tp = sp->bhdr.next;
			if (tp->len == 1 && tp->lb[0] == '.')
				break;
			if (file_aline(sp, ep, 1, m.lno, tp->lb, tp->len)) {
				rval = 1;
				goto done;
			}
		}

done:	if (rval == 0) {
		/*
		 * XXX
		 * Not sure historical ex set autoprint for change/append.
		 * Hack for user giving us line zero and then never putting
		 * anything in the file.
		 */
		if (m.lno != 0)
			F_SET(sp, S_AUTOPRINT);

		/* Set the cursor. */
		sp->lno = m.lno;
	}

	if (set)
		O_SET(sp, O_AUTOINDENT);

	return (rval);
}
