/*-
 * Copyright (c) 1993, 1994
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
static char sccsid[] = "@(#)ex_argv.c	9.4 (Berkeley) 12/2/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
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
#include <unistd.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "excmd.h"

static int argv_alloc __P((SCR *, size_t));
static int argv_fexp __P((SCR *, EXCMDARG *,
	       char *, size_t, char *, size_t *, char **, size_t *, int));
static int argv_sexp __P((SCR *, char **, size_t *, size_t *));

/*
 * argv_init --
 *	Build  a prototype arguments list.
 */
int
argv_init(sp, excp)
	SCR *sp;
	EXCMDARG *excp;
{
	EX_PRIVATE *exp;

	exp = EXP(sp);
	exp->argsoff = 0;
	argv_alloc(sp, 1);

	excp->argv = exp->args;
	excp->argc = exp->argsoff;
	return (0);
}

/*
 * argv_exp0 --
 *	Append a string to the argument list.
 */
int
argv_exp0(sp, excp, cmd, cmdlen)
	SCR *sp;
	EXCMDARG *excp;
	char *cmd;
	size_t cmdlen;
{
	EX_PRIVATE *exp;

	exp = EXP(sp);
	argv_alloc(sp, cmdlen);
	memmove(exp->args[exp->argsoff]->bp, cmd, cmdlen);
	exp->args[exp->argsoff]->bp[cmdlen] = '\0';
	exp->args[exp->argsoff]->len = cmdlen;
	++exp->argsoff;
	excp->argv = exp->args;
	excp->argc = exp->argsoff;
	return (0);
}

/*
 * argv_exp1 --
 *	Do file name expansion on a string, and append it to the
 *	argument list.
 */
int
argv_exp1(sp, excp, cmd, cmdlen, is_bang)
	SCR *sp;
	EXCMDARG *excp;
	char *cmd;
	size_t cmdlen;
	int is_bang;
{
	EX_PRIVATE *exp;
	size_t blen, len;
	char *bp, *p, *t;

	GET_SPACE_RET(sp, bp, blen, 512);

	len = 0;
	exp = EXP(sp);
	if (argv_fexp(sp, excp, cmd, cmdlen, bp, &len, &bp, &blen, is_bang)) {
		FREE_SPACE(sp, bp, blen);
		return (1);
	}

	/* If it's empty, we're done. */
	if (len != 0) {
		for (p = bp, t = bp + len; p < t; ++p)
			if (!isblank(*p))
				break;
		if (p == t)
			goto ret;
	} else
		goto ret;

	(void)argv_exp0(sp, excp, bp, len);

ret:	FREE_SPACE(sp, bp, blen);
	return (0);
}

/*
 * argv_exp2 --
 *	Do file name and shell expansion on a string, and append it to
 *	the argument list.
 */
int
argv_exp2(sp, excp, cmd, cmdlen)
	SCR *sp;
	EXCMDARG *excp;
	char *cmd;
	size_t cmdlen;
{
	size_t blen, len, n;
	int rval;
	char *bp, *mp, *p;

	GET_SPACE_RET(sp, bp, blen, 512);

#define	SHELLECHO	"echo "
#define	SHELLOFFSET	(sizeof(SHELLECHO) - 1)
	memmove(bp, SHELLECHO, SHELLOFFSET);
	p = bp + SHELLOFFSET;
	len = SHELLOFFSET;

#if defined(DEBUG) && 0
	TRACE(sp, "file_argv: {%.*s}\n", (int)cmdlen, cmd);
#endif

	if (argv_fexp(sp, excp, cmd, cmdlen, p, &len, &bp, &blen, 0)) {
		rval = 1;
		goto err;
	}

#if defined(DEBUG) && 0
	TRACE(sp, "before shell: %d: {%s}\n", len, bp);
#endif

	/*
	 * Do shell word expansion -- it's very, very hard to figure out what
	 * magic characters the user's shell expects.  Historically, it was a
	 * union of v7 shell and csh meta characters.  We match that practice
	 * by default, so ":read \%" tries to read a file named '%'.  It would
	 * make more sense to pass any special characters through the shell,
	 * but then, if your shell was csh, the above example will behave
	 * differently in nvi than in vi.  If you want to get other characters
	 * passed through to your shell, change the "meta" option.
	 *
	 * To avoid a function call per character, we do a first pass through
	 * the meta characters looking for characters that aren't expected
	 * to be there.
	 */
	for (p = mp = O_STR(sp, O_SHELLMETA); *p != '\0'; ++p)
		if (isblank(*p) || isalnum(*p))
			break;
	if (*p != '\0') {
		for (p = bp, n = len; n > 0; --n, ++p)
			if (strchr(mp, *p) != NULL)
				break;
	} else
		for (p = bp, n = len; n > 0; --n, ++p)
			if (!isblank(*p) &&
			    !isalnum(*p) && strchr(mp, *p) != NULL)
				break;
	if (n > 0) {
		if (argv_sexp(sp, &bp, &blen, &len)) {
			rval = 1;
			goto err;
		}
		p = bp;
	} else {
		p = bp + SHELLOFFSET;
		len -= SHELLOFFSET;
	}

#if defined(DEBUG) && 0
	TRACE(sp, "after shell: %d: {%s}\n", len, bp);
#endif

	rval = argv_exp3(sp, excp, p, len);

err:	FREE_SPACE(sp, bp, blen);
	return (rval);
}

/*
 * argv_exp3 --
 *	Take a string and break it up into an argv, which is appended
 *	to the argument list.
 */
int
argv_exp3(sp, excp, cmd, cmdlen)
	SCR *sp;
	EXCMDARG *excp;
	char *cmd;
	size_t cmdlen;
{
	EX_PRIVATE *exp;
	size_t len;
	int ch, off;
	char *ap, *p;

	for (exp = EXP(sp); cmdlen > 0; ++exp->argsoff) {
		/* Skip any leading whitespace. */
		for (; cmdlen > 0; --cmdlen, ++cmd) {
			ch = *cmd;
			if (!isblank(ch))
				break;
		}
		if (cmdlen == 0)
			break;

		/*
		 * Determine the length of this whitespace delimited
		 * argument.
		 *
		 * QUOTING NOTE:
		 *
		 * Skip any character preceded by the user's quoting
		 * character.
		 */
		for (ap = cmd, len = 0; cmdlen > 0; ++cmd, --cmdlen, ++len) {
			ch = *cmd;
			if (IS_ESCAPE(sp, ch) && cmdlen > 1) {
				++cmd;
				--cmdlen;
			} else if (isblank(ch))
				break;
		}

		/*
		 * Copy the argument into place.
		 *
		 * QUOTING NOTE:
		 *
		 * Lose quote chars.
		 */
		argv_alloc(sp, len);
		off = exp->argsoff;
		exp->args[off]->len = len;
		for (p = exp->args[off]->bp; len > 0; --len, *p++ = *ap++)
			if (IS_ESCAPE(sp, *ap))
				++ap;
		*p = '\0';
	}
	excp->argv = exp->args;
	excp->argc = exp->argsoff;

#if defined(DEBUG) && 0
	for (cnt = 0; cnt < exp->argsoff; ++cnt)
		TRACE(sp, "arg %d: {%s}\n", cnt, exp->argv[cnt]);
#endif
	return (0);
}

/*
 * argv_fexp --
 *	Do file name and bang command expansion.
 */
static int
argv_fexp(sp, excp, cmd, cmdlen, p, lenp, bpp, blenp, is_bang)
	SCR *sp;
	EXCMDARG *excp;
	char *cmd, *p, **bpp;
	size_t cmdlen, *lenp, *blenp;
	int is_bang;
{
	EX_PRIVATE *exp;
	char *bp, *t;
	size_t blen, len, tlen;

	/* Replace file name characters. */
	for (bp = *bpp, blen = *blenp, len = *lenp; cmdlen > 0; --cmdlen, ++cmd)
		switch (*cmd) {
		case '!':
			if (!is_bang)
				goto ins_ch;
			exp = EXP(sp);
			if (exp->lastbcomm == NULL) {
				msgq(sp, M_ERR,
				    "123|No previous command to replace \"!\"");
				return (1);
			}
			len += tlen = strlen(exp->lastbcomm);
			ADD_SPACE_RET(sp, bp, blen, len);
			memmove(p, exp->lastbcomm, tlen);
			p += tlen;
			F_SET(excp, E_MODIFY);
			break;
		case '%':
			if ((t = sp->frp->name) == NULL) {
				msgq(sp, M_ERR,
				    "124|No filename to substitute for %%");
				return (1);
			}
			tlen = strlen(t);
			len += tlen;
			ADD_SPACE_RET(sp, bp, blen, len);
			memmove(p, t, tlen);
			p += tlen;
			F_SET(excp, E_MODIFY);
			break;
		case '#':
			if ((t = sp->alt_name) == NULL) {
				msgq(sp, M_ERR,
				    "125|No filename to substitute for #");
				return (1);
			}
			len += tlen = strlen(t);
			ADD_SPACE_RET(sp, bp, blen, len);
			memmove(p, t, tlen);
			p += tlen;
			F_SET(excp, E_MODIFY);
			break;
		case '\\':
			/*
			 * QUOTING NOTE:
			 *
			 * Strip any backslashes that protected the file
			 * expansion characters.
			 */
			if (cmdlen > 1 &&
			    (cmd[1] == '%' || cmd[1] == '#' || cmd[1] == '!')) {
				++cmd;
				--cmdlen;
			}
			/* FALLTHROUGH */
		default:
ins_ch:			++len;
			ADD_SPACE_RET(sp, bp, blen, len);
			*p++ = *cmd;
		}

	/* Nul termination. */
	++len;
	ADD_SPACE_RET(sp, bp, blen, len);
	*p = '\0';

	/* Return the new string length, buffer, buffer length. */
	*lenp = len - 1;
	*bpp = bp;
	*blenp = blen;
	return (0);
}

/*
 * argv_alloc --
 *	Make more space for arguments.
 */
static int
argv_alloc(sp, len)
	SCR *sp;
	size_t len;
{
	ARGS *ap;
	EX_PRIVATE *exp;
	int cnt, off;

	/*
	 * Allocate room for another argument, always leaving
	 * enough room for an ARGS structure with a length of 0.
	 */
#define	INCREMENT	20
	exp = EXP(sp);
	off = exp->argsoff;
	if (exp->argscnt == 0 || off + 2 >= exp->argscnt - 1) {
		cnt = exp->argscnt + INCREMENT;
		REALLOC(sp, exp->args, ARGS **, cnt * sizeof(ARGS *));
		if (exp->args == NULL) {
			(void)argv_free(sp);
			goto mem;
		}
		memset(&exp->args[off], 0, INCREMENT * sizeof(ARGS *));
		exp->argscnt = cnt;
	}

	/* First argument. */
	if (exp->args[off] == NULL) {
		CALLOC(sp, exp->args[off], ARGS *, 1, sizeof(ARGS));
		if (exp->args[off] == NULL)
			goto mem;
	}

	/* First argument buffer. */
	ap = exp->args[off];
	ap->len = 0;
	if (ap->blen < len + 1) {
		ap->blen = len + 1;
		REALLOC(sp, ap->bp, CHAR_T *, ap->blen * sizeof(CHAR_T));
		if (ap->bp == NULL) {
			ap->bp = NULL;
			ap->blen = 0;
			F_CLR(ap, A_ALLOCATED);
mem:			msgq(sp, M_SYSERR, NULL);
			return (1);
		}
		F_SET(ap, A_ALLOCATED);
	}

	/* Second argument. */
	if (exp->args[++off] == NULL) {
		CALLOC(sp, exp->args[off], ARGS *, 1, sizeof(ARGS));
		if (exp->args[off] == NULL)
			goto mem;
	}
	/* 0 length serves as end-of-argument marker. */
	exp->args[off]->len = 0;
	return (0);
}

/*
 * argv_free --
 *	Free up argument structures.
 */
int
argv_free(sp)
	SCR *sp;
{
	EX_PRIVATE *exp;
	int off;

	exp = EXP(sp);
	if (exp->args != NULL) {
		for (off = 0; off < exp->argscnt; ++off) {
			if (exp->args[off] == NULL)
				continue;
			if (F_ISSET(exp->args[off], A_ALLOCATED))
				free(exp->args[off]->bp);
			FREE(exp->args[off], sizeof(ARGS));
		}
		FREE(exp->args, exp->argscnt * sizeof(ARGS *));
	}
	exp->args = NULL;
	exp->argscnt = 0;
	exp->argsoff = 0;
	return (0);
}

/*
 * argv_sexp --
 *	Fork a shell, pipe a command through it, and read the output into
 *	a buffer.
 */
static int
argv_sexp(sp, bpp, blenp, lenp)
	SCR *sp;
	char **bpp;
	size_t *blenp, *lenp;
{
	FILE *efp, *ifp;
	pid_t pid;
	size_t blen, len;
	int ch, nf1, nf2, rval, err_output[2], std_output[2];
	char *bp, *p, *t, *sh, *sh_path;

	bp = *bpp;
	blen = *blenp;

	sh_path = O_STR(sp, O_SHELL);
	if ((sh = strrchr(sh_path, '/')) == NULL)
		sh = sh_path;
	else
		++sh;

	/*
	 * There are two different processes running through this code, named
	 * the utility (the shell) and the parent. The utility reads standard
	 * input and writes standard output and standard error output.  The
	 * parent reads the standard output and standard error output, copying
	 * the former into a buffer and the latter into the error message log.
	 *
	 * The parent reads std_output[0] and err_output[0], and the utility
	 * writes std_output[1] and err_output[1].
	 */
	ifp = efp = NULL;
	std_output[0] = std_output[1] = err_output[0] = err_output[1] = -1;
	if (pipe(std_output) < 0 || pipe(err_output)) {
		msgq(sp, M_SYSERR, "pipe");
		goto err;
	}
	if ((ifp = fdopen(std_output[0], "r")) == NULL ||
	    (efp = fdopen(err_output[0], "r")) == NULL) {
		msgq(sp, M_SYSERR, "fdopen");
		goto err;
	}

	/*
	 * Do the minimal amount of work possible, the shell is going
	 * to run briefly and then exit.  We hope.
	 */
	SIGBLOCK(sp->gp);
	switch (pid = vfork()) {
	case -1:			/* Error. */
		SIGUNBLOCK(sp->gp);
		msgq(sp, M_SYSERR, "vfork");
err:		if (ifp != NULL)
			(void)fclose(ifp);
		else if (std_output[0] != -1)
			close(std_output[0]);
		if (efp != NULL)
			(void)fclose(efp);
		else if (err_output[0] != -1)
			close(std_output[0]);
		if (std_output[1] != -1)
			close(std_output[0]);
		if (err_output[1] != -1)
			close(std_output[0]);
		return (1);
	case 0:				/* Utility. */
		/* The utility has default signal behavior. */
		sig_end(sp);

		/* Redirect stdout/stderr to the write end of the pipe. */
		(void)dup2(std_output[1], STDOUT_FILENO);
		(void)dup2(err_output[1], STDERR_FILENO);

		/* Close the utility's file descriptors. */
		(void)close(std_output[0]);
		(void)close(std_output[1]);
		(void)close(err_output[0]);
		(void)close(err_output[1]);

		/* Assume that all shells have -c. */
		execl(sh_path, sh, "-c", bp, NULL);
		p = msg_print(sp, sh_path, &nf1);
		msgq(sp, M_SYSERR, "126|Error: execl: %s", p);
		if (nf1)
			FREE_SPACE(sp, p, 0);
		_exit(127);
	default:			/* Parent. */
		SIGUNBLOCK(sp->gp);

		/* Close the pipe ends the parent won't use. */
		(void)close(std_output[1]);
		(void)close(err_output[1]);
		break;
	}

	rval = 0;

	/*
	 * Copy process standard output into a buffer.
	 *
	 * !!!
	 * Historic vi apparently discarded leading \n and \r's from
	 * the shell output stream.  We don't on the grounds that any
	 * shell that does that is broken.
	 */
	for (p = bp, len = 0, ch = EOF;
	    (ch = getc(ifp)) != EOF; *p++ = ch, --blen, ++len)
		if (blen < 5) {
			ADD_SPACE_GOTO(sp, bp, blen, *blenp * 2);
			p = bp + len;
			blen = *blenp - len;
		}

	/* Delete the final newline, nul terminate the string. */
	if (p > bp && (p[-1] == '\n' || p[-1] == '\r')) {
		--len;
		*--p = '\0';
	} else
		*p = '\0';
	*lenp = len;
	*bpp = bp;		/* *blenp is already updated. */

	if (ferror(ifp))
		goto ioerr;

	/*
	 * Copy process standard error into a buffer.  We don't set
	 * rval if we get error output -- as long as the shell exits
	 * okay, might as well go ahead.
	 */
	if ((ch = getc(efp)) != EOF) {
		(void)ungetc(ch, efp);
		GET_SPACE_RET(sp, bp, blen, 1024);

		p = msg_print(sp, sh, &nf1);
		while (fgets(bp, blen, efp) != NULL) {
			if ((t = strchr(bp, '\n')) != NULL)
				*t = '\0';
			t = msg_print(sp, bp, &nf2);
			msgq(sp, M_ERR, "%s: %s", p, t);
			if (nf2)
				FREE_SPACE(sp, t, 0);
		}
		if (nf1)
			FREE_SPACE(sp, p, 0);
		FREE_SPACE(sp, bp, blen);
	}

	if (ferror(efp)) {
ioerr:		p = msg_print(sp, sh, &nf1);
		msgq(sp, M_ERR, "127|I/O error: %s", p);
		if (nf1)
			FREE_SPACE(sp, p, 0);
binc_err:	rval = 1;
	}

	/* Wait for the process. */
	if (proc_wait(sp, (long)pid, sh, 0))
		rval = 1;

	if (ifp != NULL)
		(void)fclose(ifp);
	if (efp != NULL)
		(void)fclose(efp);

	return (rval);
}
