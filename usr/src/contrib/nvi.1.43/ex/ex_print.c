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
static char sccsid[] = "@(#)ex_print.c	9.3 (Berkeley) 11/13/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
#include <sys/time.h>

#include <bitstring.h>
#include <ctype.h>
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

static int ex_prchars __P((SCR *, const char **lpp, size_t *, size_t, int));

/*
 * ex_list -- :[line [,line]] l[ist] [count] [flags]
 *
 *	Display the addressed lines such that the output is unambiguous.
 */
int
ex_list(sp, cmdp)
	SCR *sp;
	EXCMDARG *cmdp;
{
	NEEDFILE(sp, cmdp->cmd);

	if (ex_print(sp, &cmdp->addr1, &cmdp->addr2, cmdp->flags | E_F_LIST))
		return (1);
	sp->lno = cmdp->addr2.lno;
	sp->cno = cmdp->addr2.cno;
	return (0);
}

/*
 * ex_number -- :[line [,line]] nu[mber] [count] [flags]
 *
 *	Display the addressed lines with a leading line number.
 */
int
ex_number(sp, cmdp)
	SCR *sp;
	EXCMDARG *cmdp;
{
	NEEDFILE(sp, cmdp->cmd);

	if (ex_print(sp, &cmdp->addr1, &cmdp->addr2, cmdp->flags | E_F_HASH))
		return (1);
	sp->lno = cmdp->addr2.lno;
	sp->cno = cmdp->addr2.cno;
	return (0);
}

/*
 * ex_pr -- :[line [,line]] p[rint] [count] [flags]
 *
 *	Display the addressed lines.
 */
int
ex_pr(sp, cmdp)
	SCR *sp;
	EXCMDARG *cmdp;
{
	NEEDFILE(sp, cmdp->cmd);

	if (ex_print(sp, &cmdp->addr1, &cmdp->addr2, cmdp->flags))
		return (1);
	sp->lno = cmdp->addr2.lno;
	sp->cno = cmdp->addr2.cno;
	return (0);
}

/*
 * ex_print --
 *	Print the selected lines.
 */
int
ex_print(sp, fp, tp, flags)
	SCR *sp;
	MARK *fp, *tp;
	register int flags;
{
	const char *p;
	recno_t from, to;
	size_t col, len;
	char buf[10];

	for (from = fp->lno, to = tp->lno; from <= to; ++from) {
		col = 0;

		/*
		 * Display the line number.  The %6 format is specified
		 * by POSIX 1003.2, and is almost certainly large enough.
		 * Check, though, just in case.
		 */
		if (LF_ISSET(E_F_HASH)) {
			if (from <= 999999) {
				snprintf(buf, sizeof(buf), "%6ld  ", from);
				p = buf;
			} else
				p = "TOOBIG  ";
			if (ex_prchars(sp, &p, &col, 8, 0))
				return (1);
		}

		/*
		 * Display the line.  The format for E_F_PRINT isn't very good,
		 * especially in handling end-of-line tabs, but they're almost
		 * backward compatible.
		 */
		if ((p = file_gline(sp, from, &len)) == NULL) {
			GETLINE_ERR(sp, from);
			return (1);
		}

		if (len == 0 && !LF_ISSET(E_F_LIST)) {
			F_SET(sp, S_SCR_EXWROTE);
			(void)ex_printf(EXCOOKIE, "\n");
		} else if (ex_ldisplay(sp, p, len, col, flags))
			return (1);

		if (INTERRUPTED(sp))
			break;
	}
	return (0);
}

/*
 * ex_ldisplay --
 *	Display a line without any preceding number.
 */
int
ex_ldisplay(sp, lp, len, col, flags)
	SCR *sp;
	const char *lp;
	size_t len, col;
	u_int flags;
{
	const char *p;

	if (len > 0 && ex_prchars(sp, &lp, &col, len, 0))
		return (1);
	if (!INTERRUPTED(sp) && LF_ISSET(E_F_LIST)) {
		p = "$";
		if (ex_prchars(sp, &p, &col, 1, 0))
			return (1);
	}
	if (!INTERRUPTED(sp))
		(void)ex_printf(EXCOOKIE, "\n");
	return (0);
}

/*
 * ex_scprint --
 *	Display a line for the substitute with confirmation routine.
 */
int
ex_scprint(sp, fp, tp)
	SCR *sp;
	MARK *fp, *tp;
{
	const char *p;
	size_t col, len;

	col = 0;
	if (O_ISSET(sp, O_NUMBER)) {
		p = "        ";
		if (ex_prchars(sp, &p, &col, 8, 0))
			return (1);
	}

	if ((p = file_gline(sp, fp->lno, &len)) == NULL) {
		GETLINE_ERR(sp, fp->lno);
		return (1);
	}

	if (ex_prchars(sp, &p, &col, fp->cno, ' '))
		return (1);
	if (!INTERRUPTED(sp) &&
	    ex_prchars(sp, &p, &col, tp->cno - fp->cno, '^'))
		return (1);
	if (!INTERRUPTED(sp)) {
		p = "[ynq]";
		if (ex_prchars(sp, &p, &col, 5, 0))
			return (1);
	}
	(void)fflush(sp->stdfp);
	return (0);
}

/*
 * ex_prchars --
 *	Local routine to dump characters to the screen.
 */
static int
ex_prchars(sp, lpp, colp, len, rep)
	SCR *sp;
	const char **lpp;
	size_t *colp, len;
	int rep;
{
	const char *p;
	size_t col, tlen, ts;
	CHAR_T ch, *kp;

	ts = O_VAL(sp, O_TABSTOP);
	for (p = *lpp, col = *colp; len--;) {
		if ((ch = *p++) == '\t' && !O_ISSET(sp, O_LIST))
			for (tlen = ts - col % ts;
			    col < sp->cols && tlen--; ++col)
				(void)ex_printf(EXCOOKIE,
				    "%c", rep ? rep : ' ');
		else {
			kp = KEY_NAME(sp, ch);
			tlen = KEY_LEN(sp, ch);
			if (!rep  && col + tlen < sp->cols) {
				(void)ex_printf(EXCOOKIE, "%s", kp);
				col += tlen;
			} else
				for (; tlen--; ++kp, ++col) {
					if (col == sp->cols) {
						col = 0;
						(void)ex_printf(EXCOOKIE, "\n");
					}
					(void)ex_printf(EXCOOKIE,
					    "%c", rep ? rep : *kp);
				}
		}
		F_SET(sp, S_SCR_EXWROTE);
		if (INTERRUPTED(sp))
			break;
	}
	*lpp = p;
	*colp = col;
	return (0);
}
