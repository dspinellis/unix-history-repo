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
static char sccsid[] = "@(#)step.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

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

    addr = pc;
    do {
	addr = nextaddr(addr, isnext);
	line = linelookup(addr);
    } while (line == 0 && !ss_instructions);
    stepto(addr);
    curline = line;
}
