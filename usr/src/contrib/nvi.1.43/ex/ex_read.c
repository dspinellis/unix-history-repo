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
static char sccsid[] = "@(#)ex_read.c	9.5 (Berkeley) 12/1/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <bitstring.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_read --	:read [file]
 *		:read [!cmd]
 * Read from a file or utility.
 *
 * !!!
 * Historical vi wouldn't undo a filter read, for no apparent reason.
 */
int
ex_read(sp, cmdp)
	SCR *sp;
	EXCMDARG *cmdp;
{
	enum { R_ARG, R_EXPANDARG, R_FILTER } which;
	struct stat sb;
	CHAR_T *arg, *name;
	EX_PRIVATE *exp;
	FILE *fp;
	FREF *frp;
	MARK rm;
	recno_t nlines;
	size_t arglen, blen, len;
	int argc, btear, nf, rval;
	char *p;

	/*
	 * 0 args: read the current pathname.
	 * 1 args: check for "read !arg".
	 */
	switch (cmdp->argc) {
	case 0:
		which = R_ARG;
		break;
	case 1:
		arg = cmdp->argv[0]->bp;
		arglen = cmdp->argv[0]->len;
		if (*arg == '!') {
			++arg;
			--arglen;
			which = R_FILTER;
		} else
			which = R_EXPANDARG;
		break;
	default:
		abort();
		/* NOTREACHED */
	}

	/* Load a temporary file if no file being edited. */
	if (sp->ep == NULL) {
		if ((frp = file_add(sp, NULL)) == NULL)
			return (1);
		if (file_init(sp, frp, NULL, 0))
			return (1);
	}

	switch (which) {
	case R_FILTER:
		/*
		 * File name and bang expand the user's argument.  If
		 * we don't get an additional argument, it's illegal.
		 */
		argc = cmdp->argc;
		if (argv_exp1(sp, cmdp, arg, arglen, 1))
			return (1);
		if (argc == cmdp->argc)
			goto usage;
		argc = cmdp->argc - 1;

		/* Set the last bang command. */
		exp = EXP(sp);
		if (exp->lastbcomm != NULL)
			free(exp->lastbcomm);
		if ((exp->lastbcomm =
		    strdup(cmdp->argv[argc]->bp)) == NULL) {
			msgq(sp, M_SYSERR, NULL);
			return (1);
		}

		/* Redisplay the user's argument if it's changed. */
		if (F_ISSET(cmdp, E_MODIFY) && IN_VI_MODE(sp)) {
			len = cmdp->argv[argc]->len;
			GET_SPACE_RET(sp, p, blen, len + 2);
			p[0] = '!';
			memmove(p + 1,
			    cmdp->argv[argc]->bp, cmdp->argv[argc]->len + 1);
			(void)sp->s_busy(sp, p);
			FREE_SPACE(sp, p, blen);
		}

		if (filtercmd(sp, &cmdp->addr1,
		    NULL, &rm, cmdp->argv[argc]->bp, FILTER_READ))
			return (1);

		/* The filter version of read set the autoprint flag. */
		F_SET(EXP(sp), EX_AUTOPRINT);

		/* If in vi mode, move to the first nonblank. */
		sp->lno = rm.lno;
		if (IN_VI_MODE(sp)) {
			sp->cno = 0;
			(void)nonblank(sp, sp->lno, &sp->cno);
		}
		return (0);
	case R_ARG:
		name = sp->frp->name;
		break;
	case R_EXPANDARG:
		if (argv_exp2(sp, cmdp, arg, arglen))
			return (1);
		/*
		 * 0/1 args: impossible.
		 *   2 args: read it.
		 *  >2 args: object, too many args.
		 */
		switch (cmdp->argc) {
		case 0:
		case 1:
			abort();
			/* NOTREACHED */
		case 2:
			name = cmdp->argv[1]->bp;
			/*
			 * !!!
			 * Historically, the read and write commands renamed
			 * "unnamed" files, or, if the file had a name, set
			 * the alternate file name.
			 */
			if (F_ISSET(sp->frp, FR_TMPFILE) &&
			    !F_ISSET(sp->frp, FR_EXNAMED)) {
				if ((p = v_strdup(sp, cmdp->argv[1]->bp,
				    cmdp->argv[1]->len)) != NULL) {
					free(sp->frp->name);
					sp->frp->name = p;
				}
				/*
				 * The file has a real name, it's no longer a
				 * temporary, clear the temporary file flags.
				 * The read-only flag follows the file name,
				 * clear it as well.
				 */
				F_CLR(sp->frp,
				    FR_RDONLY | FR_TMPEXIT | FR_TMPFILE);
				F_SET(sp->frp, FR_NAMECHANGE | FR_EXNAMED);
			} else
				set_alt_name(sp, name);
			break;
		default:
			p = msg_print(sp, cmdp->argv[0]->bp, &nf);
			msgq(sp, M_ERR,
			    "149|%s expanded into too many file names", p);
			if (nf)
				FREE_SPACE(sp, p, 0);
usage:			ex_message(sp, cmdp->cmd, EXM_USAGE);
			return (1);
		
		}
		break;
	}

	/*
	 * !!!
	 * Historically, vi did not permit reads from non-regular files,
	 * nor did it distinguish between "read !" and "read!", so there
	 * was no way to "force" it.
	 */
	if ((fp = fopen(name, "r")) == NULL || fstat(fileno(fp), &sb)) {
		p = msg_print(sp, name, &nf);
		msgq(sp, M_SYSERR, "%s", p);
		if (nf)
			FREE_SPACE(sp, p, 0);
		return (1);
	}
	if (!S_ISREG(sb.st_mode)) {
		(void)fclose(fp);
		msgq(sp, M_ERR, "150|Only regular files may be read");
		return (1);
	}

	/* Try and get a lock. */
	if (file_lock(sp, NULL, NULL, fileno(fp), 0) == LOCK_UNAVAIL)
		msgq(sp, M_ERR, "264|%s: read lock was unavailable", name);

	/* Turn on busy message. */
	btear = F_ISSET(sp, S_EXSILENT) ? 0 : !busy_on(sp, "Reading...");
	rval = ex_readfp(sp, name, fp, &cmdp->addr1, &nlines, 1);
	if (btear)
		busy_off(sp);

	/*
	 * Set the cursor to the first line read in, if anything read
	 * in, otherwise, the address.  (Historic vi set it to the
	 * line after the address regardless, but since that line may
	 * not exist we don't bother.)
	 */
	sp->lno = cmdp->addr1.lno;
	if (nlines)
		++sp->lno;

	return (rval);
}

/*
 * ex_readfp --
 *	Read lines into the file.
 */
int
ex_readfp(sp, name, fp, fm, nlinesp, success_msg)
	SCR *sp;
	char *name;
	FILE *fp;
	MARK *fm;
	recno_t *nlinesp;
	int success_msg;
{
	EX_PRIVATE *exp;
	recno_t lcnt, lno;
	size_t len;
	u_long ccnt;			/* XXX: can't print off_t portably. */
	int nf, rval;
	char *p;

	rval = 0;
	exp = EXP(sp);

	/*
	 * Add in the lines from the output.  Insertion starts at the line
	 * following the address.
	 */
	ccnt = 0;
	lcnt = 0;
	for (lno = fm->lno; !ex_getline(sp, fp, &len); ++lno, ++lcnt) {
		if (INTERRUPTED(sp)) {
			if (!success_msg)
				ex_message(sp, NULL, EXM_INTERRUPTED);
			break;
		}
		if (file_aline(sp, 1, lno, exp->ibp, len)) {
			rval = 1;
			break;
		}
		ccnt += len;
	}

	if (ferror(fp)) {
		p = msg_print(sp, name, &nf);
		msgq(sp, M_SYSERR, "%s", p);
		if (nf)
			FREE_SPACE(sp, p, 0);
		rval = 1;
	}

	if (fclose(fp)) {
		p = msg_print(sp, name, &nf);
		msgq(sp, M_SYSERR, "%s", p);
		if (nf)
			FREE_SPACE(sp, p, 0);
		return (1);
	}

	if (rval)
		return (1);

	/* Return the number of lines read in. */
	if (nlinesp != NULL)
		*nlinesp = lcnt;

	if (success_msg) {
		p = msg_print(sp, name, &nf);
		if (INTERRUPTED(sp))
			msgq(sp, M_INFO,
		    "151|Interrupted read: %s: %lu lines, %lu characters",
			    p, lcnt, ccnt);
		else
			msgq(sp, M_INFO,
			    "152|%s: %lu lines, %lu characters",
			    p, lcnt, ccnt);
		if (nf)
			FREE_SPACE(sp, p, 0);
	}
	return (0);
}
