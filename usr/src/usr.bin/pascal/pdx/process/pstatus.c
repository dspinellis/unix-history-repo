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
static char sccsid[] = "@(#)pstatus.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * process status routines
 */

#include "defs.h"
#include <signal.h>
#include "process.h"
#include "machine.h"
#include "breakpoint.h"
#include "source.h"
#include "object.h"
#include "process.rep"

/*
 * Print the status of the process.
 * This routine does not return.
 */

printstatus()
{
    if (process->signo == SIGINT) {
	isstopped = TRUE;
	printerror();
    }
    if (isbperr() && isstopped) {
	skimsource(srcfilename(pc));
	printf("stopped at ");
	printwhere(curline, cursource);
	putchar('\n');
	if (curline > 0) {
	    printlines(curline, curline);
	} else {
	    printinst(pc, pc);
	}
	erecover();
    } else {
	isstopped = FALSE;
	fixbps();
	fixintr();
	if (process->status == FINISHED) {
	    quit(0);
	} else {
	    printerror();
	}
    }
}


/*
 * Print out the "line N [in file F]" information that accompanies
 * messages in various places.
 */

printwhere(lineno, filename)
LINENO lineno;
char *filename;
{
    if (lineno > 0) {
	printf("line %d", lineno);
	if (nlhdr.nfiles > 1) {
	    printf(" in file %s", filename);
	}
    } else {
	    printf("location %d\n", pc);
    }
}

/*
 * Return TRUE if the process is finished.
 */

BOOLEAN isfinished(p)
PROCESS *p;
{
    return(p->status == FINISHED);
}
