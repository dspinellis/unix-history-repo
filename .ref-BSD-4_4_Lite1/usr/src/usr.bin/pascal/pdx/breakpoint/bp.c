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
static char sccsid[] = "@(#)bp.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Direct management of bpinfo structures.
 */

#include "defs.h"
#include "breakpoint.h"
#include "tree.h"
#include "sym.h"
#include "main.h"
#include "source.h"
#include "object.h"
#include "bp.rep"

unsigned int uniqueid;

/*
 * Add a breakpoint to the list, return a pointer to it.
 */

BPINFO *newbp(addr, type, block, cond, node, line)
ADDRESS addr;
BPTYPE type;
SYM *block;
NODE *cond;
NODE *node;
LINENO line;
{
	register BPINFO *p;

	p = alloc(1, BPINFO);
	p->bpid = ++uniqueid;
	p->bpaddr = addr;
	p->bptype = type;
	p->bpblock = block;
	p->bpcond = cond;
	p->bpnode = node;
	p->bpline = line;
	p->bpnext = bphead;
	if (option('b')) {
		printf("new bp (%d) at %d, type %d\n", p->bpid, p->bpaddr, p->bptype);
		fflush(stdout);
	}
	bphead = p;
	return(p);
}

/*
 * Add a breakpoint, but don't return anything.
 * Just for folks outside of "breakpoint" who don't know that
 * a BPINFO exists.
 */

addbp(addr, type, block, cond, node, line)
ADDRESS addr;
BPTYPE type;
SYM *block;
NODE *cond;
NODE *node;
LINENO line;
{

	(void) newbp(addr, type, block, cond, node, line);
}

/*
 * Delete a breakpoint.
 *
 * Print out a cryptic error message if it can't be found.
 */

delbp(id)
unsigned int id;
{
	register BPINFO *p, *last;

	last = NIL;
	for (p = bphead; p != NIL; p = p->bpnext) {
		if (p->bpid == id) {
			break;
		}
		last = p;
	}
	if (p == NIL) {
		error("%d unknown", id);
	}
	switch (p->bptype) {
		case ALL_ON:
			if (p->bpline >= 0) {
				tracing--;
			} else {
				inst_tracing--;
			}
			break;

		case STOP_ON:
			var_tracing--;
			break;
		
		default:
			/* do nothing */
			break;
	}
	if (last == NIL) {
		bphead = p->bpnext;
	} else {
		last->bpnext = p->bpnext;
	}
	tfree(p->bpcond);
	tfree(p->bpnode);
	dispose(p);
}

/*
 * Free all storage in the breakpoint table.
 */

bpfree()
{
	register BPINFO *p, *next;

	fixbps();
	for (p = bphead; p != NIL; p = next) {
		next = p->bpnext;
		tfree(p->bpcond);
		tfree(p->bpnode);
		dispose(p);
	}
	bphead = NIL;
}
