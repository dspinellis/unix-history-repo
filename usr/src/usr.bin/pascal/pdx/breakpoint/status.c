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
static char sccsid[] = "@(#)status.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Print out what's currently being traced by looking at
 * the currently active breakpoints.
 *
 * The list is in LIFO order, we print it FIFO by going recursive.
 */

#include "defs.h"
#include "breakpoint.h"
#include "tree.h"
#include "sym.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "bp.rep"

#define printnum(id)	if (!isredirected()) printf("(%d) ", id)

status()
{
	if (bphead == NIL) {
		if (!isredirected()) {
			printf("no trace's or stop's active\n");
		}
	} else {
		bpstatus(bphead);
	}
}

LOCAL bpstatus(bp)
BPINFO *bp;
{
	register BPINFO *p;
	LINENO n;
	SYM *s;
	NODE *t;
	char *trname, *stname;

	p = bp;
	if (p->bpnext != NIL) {
		bpstatus(p->bpnext);
	}
	t = p->bpnode;
	if (p->bpline >= 0) {
		n = linelookup(p->bpaddr);
		trname = "trace";
		stname = "stop";
	} else {
		n = p->bpaddr;
		trname = "tracei";
		stname = "stopi";
	}
	switch(p->bptype) {
		case INST:
			printnum(p->bpid);
			printf("%s %d", trname, n);
			break;

		case ALL_ON:
			printnum(p->bpid);
			printf("%s", trname);
			s = p->bpblock;
			if (s != program) {
				printf(" in ");
				printname(s);
			}
			break;

		case STOP_ON:
			printnum(p->bpid);
			printf("%s", stname);
			if (t != NIL) {
				printf(" ");
				prtree(t);
			}
			s = p->bpblock;
			if (s != program) {
				printf(" in ");
				printname(s);
			}
			break;

		case BLOCK_ON:
		case TERM_ON:
			s = p->bpblock;
			printnum(p->bpid);
			printf("%s ", trname);
			prtree(t);
			if (s != program) {
				printf(" in ");
				printname(s);
			}
			break;

		case AT_BP:
			printnum(p->bpid);
			printf("%s ", trname);
			prtree(t);
			printf(" at %d", p->bpline);
			break;

		case STOP_BP:
			printnum(p->bpid);
			printf("%s", stname);
			if (t != NIL) {
				printf(" ");
				prtree(t);
			} else if ((s = p->bpblock) != NIL) {
				printf(" in ");
				printname(s);
			} else if (p->bpline > 0) {
				printf(" at %d", p->bpline);
			} else {
				printf(" at %d", p->bpaddr);
			}
			break;

		/*
		 * Temporary breakpoints;
		 * return rather than break to avoid printing newline.
		 */
		case ALL_OFF:
		case CALL:
		case RETURN:
		case CALLPROC:
		case STOP_OFF:
		case BLOCK_OFF:
		case TERM_OFF:
		case END_BP:
			return;

		default:
			panic("bptype %d in bplist", p->bptype);
	}
	if (p->bpcond != NIL) {
		printf(" if ");
		prtree(p->bpcond);
	}
	printf("\n");
}

/*
 * Print the name of a symbol unambigously.
 */

LOCAL printname(s)
SYM *s;
{
	if (isambiguous(s)) {
		printwhich(s);
	} else {
		printf("%s", name(s));
	}
}
