/*-
 * Copyright (c) 1980, 1993
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
static char sccsid[] = "@(#)printerror.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Print out an execution time error.
 */

#include "defs.h"
#include <signal.h>
#include "machine.h"
#include "sym.h"
#include "process.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "pxerrors.h"
#include "process/process.rep"

#ifdef tahoe
BOOLEAN shouldrestart;
#endif

printerror()
{
    register PROCESS *p;

    p = process;
    if (p->signo != ESIGNAL && p->signo != SIGINT) {
	error("signal %d at px pc %d, lc %d", p->signo, p->pc, pc);
    }
    curline = srcline(pc);
    curfunc = whatblock(pc);
    skimsource(srcfilename(pc));
    if (p->signo == ESIGNAL) {
	printf("\nerror at ");
	printwhere(curline, cursource);
        putchar('\n');
        printlines(curline, curline);
#ifdef tahoe
	/*
	 * this px is no good; it is easier to kill it and start
	 * a new one than to make it recover...
	 * make runtime/callproc.c know it too.
	 */
	shouldrestart = TRUE;
#endif
        erecover();
    } else {
	printf("\n\ninterrupt at ");
	printwhere(curline, cursource);
        putchar('\n');
        printlines(curline, curline);
        erecover();
    }
}
