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
static char sccsid[] = "@(#)ex_errlist.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#ifndef NO_ERRLIST 
#include <sys/param.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"
#include "pathnames.h"

static FILE *errlistfp;		/* Error list descriptor. */

static char	*parse_err __P((char *, char **, u_long *));
static int	 run __P((SCR *, EXF *, EXCMDARG *, char *));

int
ex_cc(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (run(sp, ep, cmdp, O_STR(sp, O_CC)));
}

int
ex_make(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	return (run(sp, ep, cmdp, O_STR(sp, O_MAKE)));
}

static int
run(sp, ep, cmdp, cname)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
	char *cname;
{
	struct termios t;
	EXCMDARG cmd;
	int fd;
	char cb[MAXPATHLEN], tf[MAXPATHLEN];

	MODIFY_CHECK(sp, ep, 0);

	/* Close any previously open error list file. */
	if (errlistfp != NULL)
		(void)fclose(errlistfp);

	(void)strcpy(tf, _PATH_ERRLIST);
	if ((fd = mkstemp(tf)) == -1) {
		msgq(sp, M_ERR, "%s: %s", tf, strerror(errno));
		return (1);
	}
	if ((errlistfp = fdopen(fd, "r")) == NULL) {
		(void)close(fd);
		msgq(sp, M_ERR, "%s: %s", tf, strerror(errno));
		return (1);
	}

	/* Build and run the command. */
	(void)snprintf(cb,
	    sizeof(cb), "%s %s 2> %s", cname, cmdp->string, tf);
	(void)fprintf(sp->stdfp, "%s", cb);

	/* Save ex/vi terminal settings, and restore the original ones. */
	(void)tcgetattr(STDIN_FILENO, &t);
	(void)tcsetattr(STDIN_FILENO, TCSADRAIN, &sp->gp->original_termios);

	/* Start with a new line. */
	(void)write(STDOUT_FILENO, "\n", 1);

	/*
	 * System will exit with an error status if the file doesn't compile,
	 * so we have to ignore errors.
	 */
	(void)esystem(sp, _PATH_BSHELL, cb);

	/* Repaint the screen. */
	F_SET(sp, S_REFRESH);

	/* Restore ex/vi terminal settings. */
	(void)tcsetattr(STDIN_FILENO, TCSAFLUSH, &t);

	/* We have a handle on the file, delete its name. */
 	(void)unlink(tf);

	/* Run the errlist command. */
	SETCMDARG(cmd, C_ERRLIST, 0, OOBLNO, 0, 0, NULL);
	return (ex_errlist(sp, ep, &cmd));
}

int
ex_errlist(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	EXF *tep;
	u_long line;
	size_t len;
	char mbuf[20], *fname, *p;

	switch (cmdp->argc) {
	case 0:
		if (errlistfp == NULL) {
			msgq(sp, M_ERR, "No error list file.");
			return (1);
		}
		break;
	case 1:
		if (errlistfp != NULL)
			(void)fclose(errlistfp);
		if ((errlistfp = fopen((char *)cmdp->argv[0], "r")) == NULL) {
			msgq(sp, M_ERR,
			    "%s: %s", cmdp->argv[0], strerror(errno));
			return (1);
		}
		break;
	default:
		abort();
	}

	/* Find the next error message in the file. */
	while (!ex_getline(sp, errlistfp, &len)) {
		/* See if it's an error message. */
		if ((p = parse_err(sp->ibp, &fname, &line)) == NULL)
			continue;

		if (strcmp(fname, ep->name)) {
			MODIFY_CHECK(sp, ep, 0);
			if ((tep = file_get(sp, ep, fname, 1)) == NULL)
				return (1);
			sp->enext = tep;
			F_SET(sp, S_FSWITCH);
		} else
			tep = ep;

		/* Go to the right line. */
		sp->lno = line;
		sp->cno = 0;
		F_CLR(ep, F_NOSETPOS);

		/* Display the error message. */
		(void)snprintf(mbuf, sizeof(mbuf), "line %lu: ", line);
		msgq(sp, M_ERR,
		    "%s%.*s", mbuf, sp->cols - strlen(mbuf) - 1, p);
		return (0);
	}
	msgq(sp, M_ERR, "No more errors in the file.");
	(void)fclose(errlistfp);
	errlistfp = NULL;
	return (0);
}

/*
 * parse_err --
 *	Try and parse a compiler error message.
 *
 *	This code handles gcc output, but fairly badly.  Gcc has a penchant
 *	for multi-line output which we don't handle at all.
 */
static char *
parse_err(line, fnamep, lnop)
	char *line, **fnamep;
	recno_t *lnop;
{
	static char *fname;
	register char *p, *t;
	int len;

	/*
	 * Grab the file name.  Ignore any associated path, the chances of
	 * two files with the same name being edited in the same session
	 * in different directories is much less than the chance of having
	 * two different paths for the same file.
	 */
	if ((p = strchr(line, ':')) == NULL)
		return (NULL);
	*p = '\0';
	if ((t = strrchr(line, '/')) == NULL)
		t = line;
	else
		++t;
	len = (p - t) + 1;
	if ((fname = realloc(fname, len)) == NULL)
		return (NULL);
	memmove(fname, t, len);
	fname[len] = '\0';
	*fnamep = fname;

	/* Grab the line number. */
	if (!*++p || !isdigit(*p))
		return (NULL);
	*lnop = strtoul(p, NULL, 10);

	/* Skip the line number. */
	if ((p = strchr(p, ':')) == NULL)
		return (NULL);

	/* Skip leading spaces. */
	for (; *p && isspace(*p); ++p);
	if (*p == '\0')
		return (NULL);

	return (p);
}
#endif /* NO_ERRLIST */
