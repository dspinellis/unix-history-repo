/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)setbps.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * set all breakpoints in object code
 */

#include "defs.h"
#include "breakpoint.h"
#include "process.h"
#include "machine.h"
#include "bp.rep"

setallbps()
{
	register BPINFO *p;

	for (p = bphead; p != NIL; p = p->bpnext) {
		setbp(p->bpaddr);
	}
}

/*
 * undo damage done by "setallbps"
 */

unsetallbps()
{
	register BPINFO *p;

	for (p = bphead; p != NIL; p = p->bpnext) {
		unsetbp(p->bpaddr);
	}
}
