/*-
 * Copyright (c) 1991, 1993
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
static char sccsid[] = "@(#)filter.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>
#include <paths.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"

/*
 * filtercmd --
 *	Run a range of lines through a filter program and replace the
 *	original text with the stdout/stderr output of the filter.
 */
int
filtercmd(sp, ep, fm, tm, rp, cmd, ftype)
	SCR *sp;
	EXF *ep;
	MARK *fm, *tm, *rp;
	char *cmd;
	enum filtertype ftype;
{
	FILE *ifp, *ofp;		/* GCC: can't be uninitialized. */
	pid_t pid;
	sig_ret_t intsave, quitsave;
	sigset_t bmask, omask;
	recno_t lno;
	size_t len;
	int input[2], output[2], pstat, rval;
	char *name;

	/* Input and output are named from the child's point of view. */
	input[0] = input[1] = output[0] = output[1] = -1;

	/* Set default cursor position. */
	*rp = *fm;

	/*
	 * If child isn't supposed to want input or send output, redirect
	 * from or to /dev/null.  Otherwise open up the pipe and get a stdio
	 * buffer for it.
	 */
	if (ftype == NOINPUT) {
		if ((input[0] = open(_PATH_DEVNULL, O_RDONLY, 0)) < 0) {
			msgq(sp, M_ERR,
			    "filter: %s: %s", _PATH_DEVNULL, strerror(errno));
			goto err;
		}
	} else
		if (pipe(input) < 0 ||
		    (ifp = fdopen(input[1], "w")) == NULL) {
			msgq(sp, M_ERR, "filter: %s", strerror(errno));
			goto err;
		}

	if (ftype == NOOUTPUT) {
		if ((output[1] = open(_PATH_DEVNULL, O_WRONLY, 0)) < 0) {
			msgq(sp, M_ERR,
			    "filter: %s: %s", _PATH_DEVNULL, strerror(errno));
			goto err;
		}
	} else
		if (pipe(output) < 0 ||
		    (ofp = fdopen(output[0], "r")) == NULL) {
			msgq(sp, M_ERR, "filter: %s", strerror(errno));
			goto err;
		}

	sigemptyset(&bmask);
	sigaddset(&bmask, SIGCHLD);
	(void)sigprocmask(SIG_BLOCK, &bmask, &omask);

	switch (pid = vfork()) {
	case -1:			/* Error. */
		(void)sigprocmask(SIG_SETMASK, &omask, NULL);
		msgq(sp, M_ERR, "filter: %s", strerror(errno));
err:		if (input[0] != -1)
			(void)close(input[0]);
		if (input[0] != -1)
			(void)close(input[0]);
		if (output[0] != -1)
			(void)close(output[0]);
		if (output[1] != -1)
			(void)close(input[1]);
		return (1);
		/* NOTREACHED */
	case 0:				/* Child. */
		(void)sigprocmask(SIG_SETMASK, &omask, NULL);

		/*
		 * Redirect stdin from the read end of the input pipe,
		 * and redirect stdout/stderr to the write end of the
		 * output pipe.
		 */
		(void)dup2(input[0], STDIN_FILENO);
		(void)dup2(output[1], STDOUT_FILENO);
		(void)dup2(output[1], STDERR_FILENO);

		/* Close the child's pipe file descriptors. */
		(void)close(input[0]);
		if (ftype != NOINPUT)
			(void)close(input[1]);
		if (ftype != NOOUTPUT)
			(void)close(output[0]);
		(void)close(output[1]);

		if ((name = strrchr(O_STR(sp, O_SHELL), '/')) == NULL)
			name = O_STR(sp, O_SHELL);
		else
			++name;
		execl(O_STR(sp, O_SHELL), name, "-c", cmd, NULL);
		msgq(sp, M_ERR,
		    "exec: %s: %s", O_STR(sp, O_SHELL), strerror(errno));
		_exit (1);
		/* NOTREACHED */
	}

	/* Close the pipe ends the parent won't use. */
	(void)close(input[0]);
	(void)close(output[1]);

	/*
	 * Write the selected lines to the write end of the input pipe.
	 * Ifp is closed by ex_writefp.
	 */
	rval = 0;
	if (ftype != NOINPUT)
		if (ex_writefp(sp, ep, "filter", ifp, fm, tm, 0))
			rval = 1;
		else {
			/* Delete old text, if any. */
			for (lno = tm->lno; lno >= fm->lno; --lno)
				if (file_dline(sp, ep, lno))
					rval = 1;
			sp->rptlines[L_DELETED] += (tm->lno - fm->lno) + 1;
		}

	if (rval == 0) {
		/*
		 * Read the output from the read end of the output pipe.
		 * Decrement the line number, ex_readfp appends to the
		 * MARK.  Ofp is closed by ex_readfp.  Set the cursor to
		 * the first line read in.
		 */
		if (ftype != NOOUTPUT) {
			rp->lno = fm->lno;
			--fm->lno;
			rval = ex_readfp(sp, ep,
			    "filter", ofp, fm, &sp->rptlines[L_ADDED]);
		}
	}

	/* Wait for the child to finish. */
	intsave = signal(SIGINT, SIG_IGN);
	quitsave = signal(SIGQUIT, SIG_IGN);
	(void)waitpid(pid, &pstat, 0);
	(void)sigprocmask(SIG_SETMASK, &omask, NULL);
	(void)signal(SIGINT, intsave);
	(void)signal(SIGQUIT, quitsave);

	if (WIFSIGNALED(pstat)) {
		len = strlen(cmd);
		msgq(sp, M_ERR,
		    "%.*s%s: exited with signal %d%s.",
		    MIN(len, 10), cmd, len > 10 ? "..." : "",
		    WTERMSIG(pstat), WCOREDUMP(pstat) ? "; core dumped" : "");
		return (1);
	} else if (WIFEXITED(pstat) && WEXITSTATUS(pstat)) {
		len = strlen(cmd);
		msgq(sp, M_ERR, "%.*s%s: exited with status %d",
		    MIN(len, 10), cmd,
		    len > 10 ? "..." : "", WEXITSTATUS(pstat));
		return (1);
	}
	return (0);
}
