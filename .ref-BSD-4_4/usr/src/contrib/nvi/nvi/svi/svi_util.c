/*-
 * Copyright (c) 1993
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
static char sccsid[] = "@(#)svi_util.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <curses.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include "vi.h"
#include "svi_screen.h"

/*
 * bell_putchar --
 *	Functional version of putchar, for tputs.
 */
static void
bell_putchar(ch)
	int ch;
{
	(void)putchar(ch);
}

/*
 * svi_bell --
 *	Ring the bell.
 */
void
svi_bell(sp)
	SCR *sp;
{
	if (O_ISSET(sp, O_FLASH)) {
		(void)tputs(sp->VB, 1, bell_putchar);
		(void)fflush(stdout);
	} else
		(void)write(STDOUT_FILENO, "\007", 1);	/* '\a' */
	F_CLR(sp, S_BELLSCHED);
}

/*
 * svi_busy_cursor --
 *	Put the cursor somewhere so the user will think we're busy.
 */
int
svi_busy_cursor(sp, msg)
	SCR *sp;
	char *msg;
{
	MOVE(sp, INFOLINE(sp), 0);
	if (msg)
		ADDSTR(msg);
	refresh();
	F_SET(sp, S_CUR_INVALID);
	return (0);
}

/*
 * svi_suspend --
 *	Suspend the svi screen; don't kill the process group, curses is
 *	expected to do that for us.
 */
int
svi_suspend(sp)
	SCR *sp;
{
	if (kill(getpid(), SIGTSTP)) {
		msgq(sp, M_ERR, "Error: SIGTSTP: %s", strerror(errno));
		return (1);
	}
	return (0);
}

/*
 * svi_gdbrefresh --
 *	Stub routine so can step through screen changes.
 */
#ifdef DEBUG
int
svi_gdbrefresh()
{
	refresh();
	return (0);
}

void
svi_gdbmap(sp)
	SCR *sp;
{
	svi_sm_dmap(sp, "gdb");
}

void
svi_sm_dmap(sp, msg)
	SCR *sp;
	char *msg;
{
	size_t cnt;
	SMAP *p;

	TRACE(sp, "==>  %s\n", msg);
	for (p = HMAP, cnt = 1; p <= TMAP; ++p, ++cnt)
		TRACE(sp, "%s<%02lu:%u> ",
		    cnt % 10 == 0 ? "\n" : "", p->lno, p->off);
	TRACE(sp, "\n");
}
#endif
