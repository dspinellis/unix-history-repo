/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)setbps.c 1.1 %G%";

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
