/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)step.c	5.1 (Berkeley) %G%";
#endif not lint
/*
 * Continue execution up to the next source line.
 *
 * We call "nextaddr" from the machine module to figure out
 * what the object address is that corresponds to the next source line.
 * If nextaddr returns -1, then the end of the program has been reached.
 *
 * There are two ways to define the next source line depending on what
 * is desired when a procedure or function call is encountered.  Step
 * stops at the beginning of the procedure or call; next skips over it.
 */

#include "defs.h"
#include "process.h"
#include "machine.h"
#include "breakpoint.h"
#include "source.h"
#include "mappings.h"
#include "process.rep"

#   if (isvax)
#       include "machine/vaxops.h"

	LOCAL ADDRESS getcall();
#   endif

/*
 * Stepc is what is called when the step command is given.
 * It has to play with the "isstopped" information.
 */

stepc()
{
    if (!isstopped) {
	error("can't continue execution");
    }
    isstopped = FALSE;
    dostep(FALSE);
    isstopped = TRUE;
}

next()
{
    if (!isstopped) {
	error("can't continue execution");
    }
    isstopped = FALSE;
    dostep(TRUE);
    isstopped = TRUE;
}

step()
{
    dostep(FALSE);
}

/*
 * Resume execution up to the given address.  It is assumed that
 * no breakpoints exist between the current address and the one
 * we're stepping to.  This saves us from setting all the breakpoints.
 */

stepto(addr)
ADDRESS addr;
{
    setbp(addr);
    resume();
    unsetbp(addr);
    if (!isbperr()) {
	printstatus();
    }
}

LOCAL dostep(isnext)
BOOLEAN isnext;
{
    register ADDRESS addr;
    register LINENO line;
    char *filename;

    addr = pc;
    do {
#       if (isvaxpx)
	    addr = nextaddr(addr, isnext);
#       else
	    if (isnext && (addr = getcall(addr)) != 0) {
		stepto(addr);
	    } else {
		pstep(process);
		addr = process->pc;
		pc = process->pc;
		errnum = process->signo;
		if (!isbperr()) {
		    printstatus();
		}
	    }
#       endif
	line = linelookup(addr);
    } while (line == 0 && !ss_instructions);
    stepto(addr);
    curline = line;
}

# if (isvax)

/*
 * If the current address contains a call instruction, return the
 * address of the instruction where control will return.
 *
 * This function is intentionally dependent on a particular type
 * of calling sequence.
 */

LOCAL ADDRESS getcall(addr)
ADDRESS addr;
{
    VAXOP op;

    iread(&op, addr, sizeof(addr));
    if (op == O_CALLS) {
	return(addr + 7);
    } else {
	return(0);
    }
}

# endif
