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
static char sccsid[] = "@(#)wheredump.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Print a list of currently active blocks starting with most recent.
 */

#include "defs.h"
#include "runtime.h"
#include "frame.rep"
#include "sym.h"
#include "machine.h"
#include "object.h"
#include "mappings.h"

where()
{
    FRAME *frp;
    ADDRESS prevpc;
    LINENO line;
    SYM *f;

    if (pc == 0) {
	error("program is not active");
    }
    prevpc = pc;
    for (frp = curframe(); frp != NIL; frp = nextframe(frp)) {
	f = whatblock(entry(frp));
	line = srcline(prevpc);
	printf("%s", name(f));
	printparams(f, frp);
	printf(", ");
	printwhere(line, srcfilename(prevpc));
	printf("\n");
	prevpc = frp->save_pc;
    }
    line = srcline(prevpc);
    printf("%s, ", name(program));
    printwhere(line, srcfilename(prevpc));
    printf("\n");
}

/*
 * Dump the world to the given file.
 * Like "where", but variables are dumped also.
 */

dump()
{
    FRAME *frp;
    ADDRESS prevpc;
    LINENO line;
    SYM *f;

    if (pc == 0) {
	error("program is not active");
    }
    prevpc = pc;
    for (frp = curframe(); frp != NIL; frp = nextframe(frp)) {
	f = whatblock(entry(frp));
	line = srcline(prevpc);
	printf("%s", name(f));
	printparams(f, frp);
	printf(", ");
	printwhere(line, srcfilename(prevpc));
	printf("\n");
	dumpvars(f, frp);
	putchar('\n');
	prevpc = frp->save_pc;
    }
    line = srcline(prevpc);
    printf("%s, ", name(program));
    printwhere(line, srcfilename(prevpc));
    printf("\n");
    dumpvars(program, NIL);
}
