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
static char sccsid[] = "@(#)runcont.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Execution management.
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "object.h"
#include "main.h"
#include "breakpoint.h"
#include "command.h"
#include "process.rep"

#define MAXNARGS 100        /* maximum number of arguments to RUN */

typedef char *String;

LOCAL BOOLEAN just_started;
LOCAL int argc;
LOCAL String argv[MAXNARGS];
LOCAL String infile;
LOCAL String outfile;
LOCAL PROCESS pbuf;
PROCESS *process = &pbuf;

/*
 * This is a px-related kludge to deal with the possibility
 * of object code magically coming from a tmp file.
 */

LOCAL String mode;
LOCAL String realname;

setargs(m, r)
char *m, *r;
{
    mode = m;
    realname = r;
}

/*
 * Initialize the argument list.
 */

arginit()
{
    infile = NIL;
    outfile = NIL;
    argv[0] = mode;
    argv[1] = objname;
    if (option('t') && realname == NIL) {
	argc = 2;
    } else {
	argv[2] = realname;
	argc = 3;
    }
}

/*
 * Add an argument to the list for the debuggee.
 */

newarg(arg)
String arg;
{
    if (argc >= MAXNARGS) {
	error("too many arguments to run");
    }
    argv[argc++] = arg;
}

/*
 * Set the standard input for the debuggee.
 */

inarg(filename)
String filename;
{
    if (infile != NIL) {
	error("multiple input redirects");
    }
    infile = filename;
}

/*
 * Set the standard output for the debuggee.
 * Probably should check to avoid overwriting an existing file.
 */

outarg(filename)
String filename;
{
    if (outfile != NIL) {
	error("multiple output redirect");
    }
    outfile = filename;
}

/*
 * Initial start of the process.  The idea is to get it to the point
 * where the object code has been loaded but execution has not begun.
 */

initstart()
{
    arginit();
    argv[argc] = NIL;
    initcache(process);
    start(argv, infile, outfile);
    if (process->status != STOPPED) {
	panic("could not start program");
    }
}

/*
 * Run starts debuggee executing.
 */

run()
{
    fixbps();
    curline = 0;
    argv[argc] = NIL;
    start(argv, infile, outfile);
    if (process->status == STOPPED) {
	just_started = TRUE;
	isstopped = FALSE;
	cont();
    } else if (option('r')) {
	panic("could not start program");
    }
}

/*
 * Continue execution wherever we left off.
 *
 * Note that this routine never returns.  Eventually bpact() will fail
 * and we'll call printstatus or step will call it.
 */

typedef void SIGFUN();

LOCAL SIGFUN *dbintr;
LOCAL void intr();

#define fails       == FALSE

cont()
{
    dbintr = signal(SIGINT, intr);
    if (just_started) {
	just_started = FALSE;
    } else {
	if (!isstopped) {
	    error("can't continue execution");
	}
	isstopped = FALSE;
	step();
    }
    for (;;) {
	if (single_stepping) {
	    printnews();
	} else {
	    setallbps();
	    resume();
	    unsetallbps();
	    if (bpact() fails) {
		printstatus();
	    }
	}
	step();
    }
    /* NOTREACHED */
}

/*
 * This routine is called if we get an interrupt while "running" px
 * but actually in the debugger.  Could happen, for example, while
 * processing breakpoints.
 *
 * We basically just want to keep going; the assumption is
 * that when the process resumes it will get the interrupt
 * which will then be handled.
 */

LOCAL void intr()
{
    signal(SIGINT, intr);
}

fixintr()
{
    signal(SIGINT, dbintr);
}
