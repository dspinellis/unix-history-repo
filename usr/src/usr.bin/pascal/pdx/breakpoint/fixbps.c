/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fixbps.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * fix up breakpoint information before continuing execution
 *
 * It's necessary to destroy breakpoints that were created temporarily
 * and still exist because the program terminated abnormally.
 */

#include "defs.h"
#include "breakpoint.h"
#include "bp.rep"

fixbps()
{
	register BPINFO *p, *last, *next;

	last = NIL;
	p = bphead;
	while (p != NIL) {
		next = p->bpnext;
		switch(p->bptype) {
			case ALL_OFF:
				if (p->bpline >= 0) {
					--tracing;
				} else {
					--inst_tracing;
				}
				if (p->bpcond != NIL) {
					delcond(TRPRINT, p->bpcond);
				}
				goto delete;

			case STOP_OFF:
				var_tracing--;
				delcond(TRSTOP, p->bpcond);
				goto delete;

			case TERM_OFF:
				--var_tracing;
				delvar(TRPRINT, p->bpnode, p->bpcond);
				goto delete;

			case CALL:
			case RETURN:
			case BLOCK_OFF:
			case CALLPROC:
			case END_BP:

			delete:
				if (last == NIL) {
					bphead = next;
				} else {
					last->bpnext = next;
				}
				dispose(p);
				break;

			default:
				last = p;
				break;
		}
		p = next;
	}
	tracing = 0;
	var_tracing = 0;
	inst_tracing = 0;
	trfree();
	condfree();
}
