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
static char sccsid[] = "@(#)fwopen.c	8.6 (Berkeley) 3/8/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/cdefs.h>
#include <sys/queue.h>
#include <sys/time.h>

#include <bitstring.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <termios.h>

#include <stdio.h>
#undef fwopen					/* For testing. */
#include <unistd.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "pathnames.h"

/*
 * The major portability problem in nvi is that it uses new functionality
 * from 4.4BSD to handle the interaction between vi and ex.
 *
 * Ex was written to use the stdio output routines because it's a lot easier
 * that way.  Since vi wants to take that output and put it up on the screen
 * using curses, it needs to replace the underlying read/write routines in a
 * stdio stream with its own.  The way this works is that when vi wants to
 * use the ex routines, it sets it up so that the output of ex goes to an nvi
 * function which knows how to display the output on the screen.  This way vi
 * never has to leave curses (resulting in much nicer screen displays) and ex
 * can use printf(3) without concern for what else is going on.  4.4BSD has a
 * stdio function (fwopen(3)) which provides this functionality.  Most other
 * systems don't.
 *
 * Vi/ex uses the following key strings:
 *
 *	ex_printf	-- ex printf routine
 *	EXCOOKIE	-- the cookie passed to the ex printf routine
 *	ex_fflush	-- ex flush routine
 *
 * and there are #defines (based on FWOPEN_NOT_AVAILABLE) in vi.h to set them
 * to the corresponding stdio(3) routines if fwopen(3) is available.  If it's
 * not available, this file contains the routines that fake it for you.
 */

/*
 * fwopen --
 *	Return an open file descriptor.
 */
FILE *
fwopen(sp, func)
	SCR *sp;
	void *func;
{
	return (fopen(_PATH_DEVNULL, "w"));
}

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static size_t off;
static char buf[1024];

/*
 * ex_printf --
 *	Ex's version of printf.
 *
 * XXX
 * Hard limits -- if we get more than 1024 of formatted
 * input in a write, it will be discarded.
 */
int
#ifdef __STDC__
ex_printf(SCR *sp, const char *fmt, ...)
#else
ex_printf(sp, fmt, va_alist)
	SCR *sp;
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int n;
	char b1[1024];

#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	if (sp->stdfp == stdout)
	    return (vfprintf(stdout, fmt, ap));

	n = vsnprintf(b1, sizeof(b1), fmt, ap);
	va_end(ap);

	if (n > 512 || n + off > sizeof(buf)) {
		(void)sp->s_ex_write(sp, buf, off);
		off = 0;
		if (n > 512) {
			(void)sp->s_ex_write(sp, b1, n);
			return (n);
		}
	}
	memmove(buf + off, b1, n);
	off += n;
	return (n);
}

/*
 * ex_fflush --
 *	Ex's version of fflush.
 */
int
ex_fflush(sp)
	SCR *sp;
{
	if (sp->stdfp == stdout)
		return (fflush(stdout));

	if (off) {
		(void)sp->s_ex_write(sp, buf, off);
		off = 0;
	}
	return (0);
}
