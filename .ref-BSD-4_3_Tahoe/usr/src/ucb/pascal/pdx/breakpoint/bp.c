/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bp.c	5.2 (Berkeley) 4/7/87";
#endif not lint

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
