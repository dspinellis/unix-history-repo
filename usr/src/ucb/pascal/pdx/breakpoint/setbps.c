/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)setbps.c	5.1 (Berkeley) 6/5/85";
#endif not lint
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
