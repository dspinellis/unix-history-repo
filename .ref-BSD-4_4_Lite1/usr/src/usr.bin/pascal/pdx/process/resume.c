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
static char sccsid[] = "@(#)resume.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Resume execution, first setting appropriate registers.
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "main.h"
#include "process.rep"
#include "runtime/frame.rep"

#include "machine/pxerrors.h"
#include "pxinfo.h"

/*
 * Resume execution, set (get) pcode location counter before (after) resuming.
 */

resume()
{
    register PROCESS *p;

    p = process;
    do {
	if (option('e')) {
	    printf("execution resumes at pc 0x%x, lc %d\n", process->pc, pc);
	    fflush(stdout);
	}
	pcont(p);
	dread(&pc, PCADDR, sizeof(pc));		/* Get pcode pc */
	if (option('e')) {
	    printf("execution stops at pc 0x%x, lc %d on sig %d\n",
		process->pc, pc, p->signo);
	    fflush(stdout);
	}
    } while (p->signo == SIGCONT);
    if (option('r') && p->signo != 0) {
	choose();
    }

    /*
     * If px implements a breakpoint by executing a halt instruction
     * the real pc must be incremented to skip over it.
     *
     * Currently, px sends itself a signal so no incrementing is needed.
     *
	if (isbperr()) {
	    p->pc++;
	}
     */
}

/*
 * Under the -r option, we offer the opportunity to just get
 * the px traceback and not actually enter the debugger.
 *
 * If the standard input is not a tty but standard error is,
 * change standard input to be /dev/tty.
 */

LOCAL choose()
{
    register int c;

    if (!isterm(stdin)) {
	if (!isterm(stderr) || freopen("/dev/tty", "r", stdin) == NIL) {
	    unsetsigtraces(process);
	    pcont(process);
	    quit(process->exitval);
	    /* NOTREACHED */
	}
    }
    fprintf(stderr, "\nProgram error");
    fprintf(stderr, "\nDo you wish to enter the debugger? ");
    c = getchar();
    if (c == 'n') {
	unsetsigtraces(process);
	pcont(process);
	quit(process->exitval);
    }
    while (c != '\n' && c != EOF) {
	c = getchar();
    }
    fprintf(stderr, "\nEntering debugger ...");
    init();
    option('r') = FALSE;
    fprintf(stderr, " type 'help' for help.\n");
}
