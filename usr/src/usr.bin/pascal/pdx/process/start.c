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
static char sccsid[] = "@(#)start.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Begin execution.
 *
 * For px, pstart does a traced exec to read in px and then stop.  But we
 * want control after px has read in the obj file and before it starts
 * executing.  The zeroth argument to px tells it to give us control
 * by sending itself a signal just prior to interpreting.
 *
 * We set a "END_BP" breakpoint at the end of the code so that the
 * process data doesn't disappear after the program terminates.
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "main.h"
#include "breakpoint.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "sym.h"
#include "process.rep"

#include "pxinfo.h"

start(argv, infile, outfile)
char **argv;
char *infile, *outfile;
{
    char *cmd;

    setsigtrace();
    cmd = "px";
    pstart(process, cmd, argv, infile, outfile);
    if (process->status == STOPPED) {
	TRAPARGS *ap, t;

	pcont(process);
	if (process->status != STOPPED) {
	    if (option('t')) {
		quit(process->exitval);
	    } else {
		panic("px exited with %d", process->exitval);
	    }
	}
#ifdef tahoe
	dread(&ap, process->fp, sizeof(ap));
	ap = (TRAPARGS *)((unsigned)ap - 4);
	dread(&RETLOC, process->fp - 8, sizeof(RETLOC));
#else
	dread(&ap, process->fp + 2*sizeof(int), sizeof(ap));
#endif
	dread(&t, ap, sizeof(TRAPARGS));

#define NARGS 5
#ifdef tahoe
#	define STKNARGS (sizeof(int)*(NARGS+1))
#	define NARGLOC  t.trp_removed
#else
#	define STKNARGS (NARGS)
#	define NARGLOC  t.nargs
#endif
	if (NARGLOC != STKNARGS) {
	    if (option('t')) {
		unsetsigtraces(process);
		pcont(process);
		quit(process->exitval);
	    } else {
		panic("start: args out of sync");
	    }
	}
	DISPLAY = t.disp;
	DP = t.dp;
	ENDOFF = t.objstart;
	PCADDR = t.pcaddr;
	LOOPADDR = t.loopaddr;
	pc = 0;
	curfunc = program;
	if (objsize != 0) {
	    addbp(lastaddr(), END_BP, NIL, NIL, NIL, 0);
	}
    }
}

/*
 * Note the termination of the program.  We do this so as to avoid
 * having the process exit, which would make the values of variables
 * inaccessible.
 *
 * Although the END_BP should really be deleted, it is taken
 * care of by fixbps the next time the program runs.
 */

endprogram()
{
    if (ss_variables) {
	prvarnews();
    }
    printf("\nexecution completed\n");
    curfunc = program;
    skimsource(srcfilename(pc));
    curline = lastlinenum;
    erecover();
}

/*
 * set up what signals we want to trace
 */

LOCAL setsigtrace()
{
    register PROCESS *p;

    p = process;
    psigtrace(p, SIGINT, TRUE);
    psigtrace(p, SIGTRAP, TRUE);
    psigtrace(p, SIGIOT, TRUE);
    psigtrace(p, SIGILL, TRUE);
    psigtrace(p, SIGBUS, TRUE);
}
