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
static char sccsid[] = "@(#)v_init.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <stdlib.h>
#include <string.h>
#include <paths.h>

/*
 * Uncomment the following for FWOPEN_NOT_AVAILABLE.
 *
 * #define FWOPEN_NOT_AVAILABLE
 */
#include "vi.h"
#include "vcmd.h"
#include "excmd.h"

#ifdef	FWOPEN_NOT_AVAILABLE
#include <sys/types.h>
#include <sys/syscall.h>			/* SYS_write. */

#include <unistd.h>

/*
 * The fwopen call substitutes a local routine for the write system call.
 * This allows vi to trap all of the writes done via stdio by ex.  If you
 * don't have fwopen or a similar way of replacing stdio's calls to write,
 * the following kludge may work.  It depends on your loader not realizing
 * that it's being tricked and linking the C library with the real write
 * system call.  Getting the real write system call is a bit tricky, there
 * are two possible ways listed below.
 */
int
write(fd, buf, n)
	int fd;
	const void *buf;
	size_t n;
{
	SCR *sp;

	/*
	 * Walk the list of screens, and see if anyone is trapping
	 * this file descriptor.
	 */
	for (sp = __global_list->scrhdr.next;
	    sp != (SCR *)&__global_list->scrhdr; sp = sp->next)
		if (fd == sp->trapped_fd)
			return (sp->s_ex_write(sp, buf, n));

#ifdef SYS_write
	return (syscall(SYS_write, fd, buf, n));
#else
	return (_write(fd, buf, n));
#endif
}
#endif

/*
 * v_init --
 *	Initialize vi.
 */
int
v_init(sp, ep)
	SCR *sp;
	EXF *ep;
{
	size_t len;
	int needfill;

	/* Make ex display to a special function. */
#ifdef FWOPEN_NOT_AVAILABLE
	if ((sp->stdfp = fopen(_PATH_DEVNULL, "w")) == NULL)
		return (1);
	sp->trapped_fd = fileno(sp->stdfp);
#else
	if ((sp->stdfp = fwopen(sp, sp->s_ex_write)) == NULL)
		return (1);
#endif
	(void)setvbuf(sp->stdfp, NULL, _IOLBF, 0);

	/*
	 * If no starting location specified, vi starts at the beginning.
	 * Otherwise, check to make sure that the location exists.
	 */
	needfill = 0;
	if (F_ISSET(ep, F_NOSETPOS)) {
		sp->lno = 1;
		sp->cno = 0;
		if (O_ISSET(sp, O_COMMENT) && v_comment(sp, ep))
			return (1);
		needfill = 1;
		F_CLR(ep, F_NOSETPOS);
	} else if (file_gline(sp, ep, sp->lno, &len) == NULL) {
		if (sp->lno != 1 || sp->cno != 0) {
			if (file_lline(sp, ep, &sp->lno))
				return (1);
			if (sp->lno == 0)
				sp->lno = 1;
			sp->cno = 0;
		}
		needfill = 1;
	} else if (sp->cno >= len) {
		needfill = 1;
		sp->cno = 0;
	}

	/*
	 * After location established, run any initial command.  Failure
	 * doesn't halt the session.  Don't worry about the cursor being
	 * repositioned affecting the success of this command, it's
	 * pretty unlikely.
	 */
	if (F_ISSET(ep, F_ICOMMAND)) {
		(void)ex_cstring(sp, ep, ep->icommand, strlen(ep->icommand));
		free(ep->icommand);
		needfill = 1;
		F_CLR(ep, F_ICOMMAND);
	}

	/*
	 * Now have the real location the user wants.
	 * Fill the screen map.
	 */
	if (needfill) {
		if (sp->s_fill(sp, ep, sp->lno, P_FILL))
			return (1);
		F_SET(sp, S_REDRAW);
	}

	/* Display the status line. */
	return (status(sp, ep, sp->lno, 0));
}

/*
 * v_end --
 *	End vi session.
 */
int
v_end(sp)
	SCR *sp;
{
#ifdef FWOPEN_NOT_AVAILABLE
	sp->trapped_fd = -1;
#endif
	(void)fclose(sp->stdfp);
	return (0);
}
