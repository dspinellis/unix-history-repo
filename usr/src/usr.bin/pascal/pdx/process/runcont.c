/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)runcont.c 1.1 %G%";

/*
 * execution management
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "object.h"
#include "breakpoint.h"
#include "command.h"
#include "process.rep"

#define MAXNARGS 10			/* maximum number of arguments to RUN */

typedef char *string;

LOCAL BOOLEAN just_started;
LOCAL int argc;
LOCAL string argv[MAXNARGS];
LOCAL string infile;
LOCAL string outfile;

/*
 * initialize the argument list
 */

arginit()
{
	infile = NIL;
	outfile = NIL;
#	if (isvaxpx)
		argv[0] = "px";
		argv[1] = "-d";
		argv[2] = objname;
		argc = 3;
#	else
		argv[0] = objname;
		argc = 1;
#	endif
}

/*
 * add an argument to the list for the debuggee
 */

newarg(arg)
string arg;
{
	if (argc >= MAXNARGS) {
		error("too many arguments");
	}
	argv[argc++] = arg;
}

/*
 * set the standard input for the debuggee
 */

inarg(filename)
string filename;
{
	if (infile != NIL) {
		error("multiple input redirects");
	}
	infile = filename;
}

/*
 * set the standard output for the debuggee
 * should probably check to avoid overwriting an existing file
 */

outarg(filename)
string filename;
{
	if (outfile != NIL) {
		error("multiple output redirect");
	}
	outfile = filename;
}

/*
 * run starts debuggee executing
 */

run()
{
	fixbps();
	curline = 0;
	start(argv, infile, outfile);
	just_started = TRUE;
	isstopped = FALSE;
	cont();
}

/*
 * continue execution wherever we left off
 *
 * Note that this routine never returns.  Eventually bpact() will fail
 * and we'll call printstatus or step will call it.
 */

typedef int INTFUNC();

LOCAL INTFUNC *dbintr;
LOCAL intr();

#define succeeds	== TRUE
#define fails		== FALSE

cont()
{
	dbintr = signal(SIGINT, &intr);
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
	/*NOTREACHED*/
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

LOCAL intr()
{
	signal(SIGINT, &intr);
}

fixintr()
{
	signal(SIGINT, &dbintr);
}
