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
static char sccsid[] = "@(#)system.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <paths.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "excmd.h"

int
esystem(sp, shell, command)
	SCR *sp;
	const u_char *shell, *command;
{
	pid_t pid;
	sig_ret_t intsave, quitsave;
	sigset_t bmask, omask;
	int pstat;
	const char *name;

	sigemptyset(&bmask);
	sigaddset(&bmask, SIGCHLD);
	(void)sigprocmask(SIG_BLOCK, &bmask, &omask);

	switch(pid = vfork()) {
	case -1:			/* error */
		(void)sigprocmask(SIG_SETMASK, &omask, NULL);
		msgq(sp, M_ERR, "fork: %s", strerror(errno));
		return (1);
	case 0:				/* child */
		(void)sigprocmask(SIG_SETMASK, &omask, NULL);
		if ((name = strrchr((char *)shell, '/')) == NULL)
			name = (char *)shell;
		else
			++name;
		execl((char *)shell, name, "-c", command, NULL);
		msgq(sp, M_ERR, "exec: %s: %s", shell, strerror(errno));
		_exit(1);
	}

	intsave = signal(SIGINT, SIG_IGN);
	quitsave = signal(SIGQUIT, SIG_IGN);
	pid = waitpid(pid, (int *)&pstat, 0);

	(void)sigprocmask(SIG_SETMASK, &omask, NULL);
	(void)signal(SIGINT, intsave);
	(void)signal(SIGQUIT, quitsave);

	return(pid == -1 ? -1 : WEXITSTATUS(pstat));
}

/*
 * ex_run_process --
 *	Fork a process and exec a program, reading the standard out of the
 *	program and piping it to the output of ex, whether that's really
 *	stdout, or the vi screen.
 */
int
ex_run_process(sp, cmd, lenp, bp, blen)
	SCR *sp;
	char *cmd, *bp;
	size_t *lenp, blen;
{
	FILE *iop;
	pid_t pid;
	sigset_t bmask, omask;
	size_t len;
	int ch, cnt, nr, pstat, rval, pdes[2];
	char *p, *sh, *sh_path, buf[1024];

	if (pipe(pdes) < 0) {
		msgq(sp, M_ERR, "Error: pipe: %s", strerror(errno));
		return (1);
	}

	sh_path = O_STR(sp, O_SHELL);
	if ((sh = strrchr(sh_path, '/')) == NULL)
		sh = sh_path;
	else
		++sh;

	switch (pid = vfork()) {
	case -1:			/* Error. */
		msgq(sp, M_ERR, "Error: vfork: %s", strerror(errno));
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		return (1);
	case 0:				/* Child. */
		if (pdes[1] != STDOUT_FILENO) {
			(void)dup2(pdes[1], STDOUT_FILENO);
			(void)close(pdes[1]);
		}
		if (pdes[1] != STDERR_FILENO)
			(void)dup2(STDOUT_FILENO, STDERR_FILENO);
		(void)close(pdes[0]);
		/* Use -f for the csh; assume that all shells have -c. */
		if (!strcmp(sh_path, _PATH_CSHELL))
			execl(sh_path, sh, "-f", "-c", cmd, NULL);
		else
			execl(sh_path, sh, "-c", cmd, NULL);
		msgq(sp, M_ERR, "Error: %s: %s", sh_path, strerror(errno));
		_exit(0);
	}

	/* Parent; assume fdopen can't fail. */
	iop = fdopen(pdes[0], "r");
	(void)close(pdes[1]);

	rval = 0;
	if (bp != NULL) {
		/* Copy process output into a buffer. */
		for (p = bp, len = 0;
		    --blen && (ch = getc(iop)) != EOF; *p++ = ch, ++len);
		if (ch != EOF) {
			rval = 1;
			msgq(sp, M_ERR, "%s: output truncated", sh);
		}
		if (p > bp && p[-1] == '\n' || p[-1] == '\r') {
			--len;
			*--p = '\0';
		} else
			*p = '\0';
		*lenp = len;
	} else {
		/* Copy process output to ex output, changing \r's to \n's. */
		while (nr = fread(buf, sizeof(buf[0]), sizeof(buf), iop)) {
			for (p = buf, cnt = nr; --cnt; ++p)
				if (*p == '\r')
					*p = '\n';
			if (fwrite(buf, sizeof(buf[0]), nr, sp->stdfp) != nr) {
				rval = 1;
				msgq(sp, M_ERR, "I/O error: %s", sh);
				break;
			}
		}
		(void)fflush(sp->stdfp);
	}
	if (ferror(iop)) {
		rval = 1;
		msgq(sp, M_ERR, "I/O error: %s", sh);
	}
	(void)fclose(iop);

	/* Get the status of the process. */
	sigemptyset(&bmask);
	sigaddset(&bmask, SIGINT);
	sigaddset(&bmask, SIGQUIT);
	sigaddset(&bmask, SIGHUP);
	(void)sigprocmask(SIG_BLOCK, &bmask, &omask);
	do {
		pid = waitpid(pid, (int *)&pstat, 0);
	} while (pid == -1 && errno == EINTR);
	(void)sigprocmask(SIG_SETMASK, &omask, NULL);

	if (WIFSIGNALED(pstat)) {
		if (bp != NULL)
			(void)fprintf(sp->stdfp, "%s\n", bp);
		(void)fprintf(sp->stdfp, "%s: exited with signal %d%s.\n", sh,
		    WTERMSIG(pstat), WCOREDUMP(pstat) ? "; core dumped" : "");
		return (1);
	} else if (WIFEXITED(pstat) && WEXITSTATUS(pstat)) {
		if (bp != NULL)
			(void)fprintf(sp->stdfp, "%s\n", bp);
		(void)fprintf(sp->stdfp,
		    "%s: exited with status %d.\n", sh, WEXITSTATUS(pstat));
		return (1);
	}
	return (rval);
}

#ifdef SHELL_PROCESS
/*
 * ex_run_shell --
 *	Fork a process and exec a shell, copying our input from to its
 *	standard input, reading its standard output and piping it to the
 *	output of ex, whether that's really stdout, or the vi screen.
 */
int
ex_run_shell(sp)
	SCR *sp;
{
	FILE *iop;
	pid_t pid;
	sigset_t bmask, omask;
	size_t len;
	fd_set fdset;
	int cnt, nr, pstat, cdes[2], pdes[2];
	char *p, *sh, *sh_path, buf[1024];

	/* Open the parent-to-child file descriptors. */
	if (pipe(pdes) < 0) {
		msgq(sp, M_ERR, "Error: pipe: %s", strerror(errno));
		return (1);
	}
	/* Open the child-to-parent file descriptors. */
	if (pipe(cdes) < 0) {
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		msgq(sp, M_ERR, "Error: pipe: %s", strerror(errno));
		return (1);
	}

	sh_path = O_STR(sp, O_SHELL);
	if ((sh = strrchr(sh_path, '/')) == NULL)
		sh = sh_path;
	else
		++sh;

	/*
	 *	| parent	| child
	 *	++++++++++++++++++++++++++++++
	 * pdes |X	write	|stdin	X
	 * cdes |read	X	|X	stdout
	 */
	switch (pid = vfork()) {
	case -1:			/* Error. */
		msgq(sp, M_ERR, "Error: vfork: %s", strerror(errno));
		(void)close(cdes[0]);
		(void)close(cdes[1]);
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		return (1);
	case 0:				/* Child. */
		(void)close(cdes[0]);	/* Unused. */
		(void)close(pdes[1]);
		if (pdes[0] != STDIN_FILENO) {
			(void)dup2(pdes[0], STDIN_FILENO);
			(void)close(pdes[0]);
		}
		if (cdes[1] != STDOUT_FILENO) {
			(void)dup2(cdes[1], STDOUT_FILENO);
			(void)close(cdes[1]);
		}
		if (cdes[1] != STDERR_FILENO)
			(void)dup2(STDOUT_FILENO, STDERR_FILENO);

		execl(sh_path, sh, "-i", NULL);
		msgq(sp, M_ERR, "Error: %s: %s", sh_path, strerror(errno));
		_exit(0);
	}

	(void)close(cdes[1]);		/* Parent. */
	(void)close(pdes[0]);		/* Unused. */
	
	FD_ZERO(&fdset);
	for (;;) {
		FD_SET(cdes[0], &fdset);
		FD_SET(STDIN_FILENO, &fdset);
		if (select(32, &fdset, NULL, NULL, NULL) == -1)
			goto err;
		if (FD_ISSET(cdes[0], &fdset)) {
			nr = read(cdes[0], buf, sizeof(buf));
			if (nr == 0)
				break;
			if (nr == -1)
				goto err;
			for (p = buf, cnt = 0; cnt < nr; ++cnt, ++p)
				if (*p == '\r')
					*p = '\n';
			if (fwrite(buf, sizeof(buf[0]), nr, sp->stdfp) != nr) {
err:				msgq(sp, M_ERR, "I/O error: %s: %s",
				    sh, strerror(errno));
				break;
			}
			(void)fflush(sp->stdfp);
		} else if (FD_ISSET(STDIN_FILENO, &fdset)) {
userwait:		nr = read(STDIN_FILENO, buf, sizeof(buf));
			if (nr == 0)
				break;
			if (nr == -1)
				goto err;
			for (p = buf, cnt = 0; cnt < nr; ++cnt, ++p)
				if (*p == '\r')
					*p = '\n';
			if (fwrite(buf, sizeof(buf[0]), nr, sp->stdfp) != nr)
				goto err;
			(void)fflush(sp->stdfp);
			if (write(pdes[1], buf, nr) != nr)
				goto err;
			if (buf[nr - 1] != '\n')
				goto userwait;
		}
	}
			
	(void)close(cdes[0]);
	(void)close(pdes[1]);

	/* Get the status of the process. */
	sigemptyset(&bmask);
	sigaddset(&bmask, SIGINT);
	sigaddset(&bmask, SIGQUIT);
	sigaddset(&bmask, SIGHUP);
	(void)sigprocmask(SIG_BLOCK, &bmask, &omask);
	do {
		pid = waitpid(pid, (int *)&pstat, 0);
	} while (pid == -1 && errno == EINTR);
	(void)sigprocmask(SIG_SETMASK, &omask, NULL);

	if (WIFSIGNALED(pstat)) {
		len = strlen(sh);
		(void)fprintf(sp->stdfp, "%.*s%s: exited with signal %d%s.\n",
		    (int)MIN(len, 10), len > 10 ? "..." : "", sh,
		    WTERMSIG(pstat), WCOREDUMP(pstat) ? "; core dumped" : "");
		return (1);
	} else if (WIFEXITED(pstat) && WEXITSTATUS(pstat)) {
		len = strlen(sh);
		(void)fprintf(sp->stdfp, "%.*s%s: exited with status %d.\n",
		    sh, (int)MIN(len, 10),
		    len > 10 ? "..." : "", WEXITSTATUS(pstat));
		return (1);
	}
	return (0);
}
#endif
