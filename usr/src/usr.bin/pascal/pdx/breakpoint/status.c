/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)status.c	5.1 (Berkeley) %G%";
#endif not lint
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
