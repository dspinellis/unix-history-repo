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
static char sccsid[] = "@(#)ex_shell.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/param.h>

#include <unistd.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_shell -- :sh[ell]
 *	Invoke the program named in the SHELL environment variable
 *	with the argument -i.
 */
int
ex_shell(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	struct termios t;
	int rval;
	char buf[MAXPATHLEN];

	MODIFY_WARN(sp, ep);

#ifdef SHELL_PROCESS
	if (ex_run_shell(sp))
		return (1);
#else
	/* Save ex/vi terminal settings, and restore the original ones. */
	(void)tcgetattr(STDIN_FILENO, &t);
	(void)tcsetattr(STDIN_FILENO, TCSADRAIN, &sp->gp->original_termios);

	/* Start with a new line. */
	(void)write(STDOUT_FILENO, "\n", 1);

	(void)snprintf(buf, sizeof(buf), "%s -i", O_STR(sp, O_SHELL));
	rval = esystem(sp, O_STR(sp, O_SHELL), buf);

	/* Restore ex/vi terminal settings. */
	(void)tcsetattr(STDIN_FILENO, TCSAFLUSH, &t);

	/* Repaint the screen. */
	F_SET(sp, S_REFRESH);
#endif
	return (rval);
}
