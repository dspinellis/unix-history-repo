/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PCLOSE.c 1.5 %G%";

/*
 * Close all files associated with the topmost stack frame.
 */

#include "h00vars.h"
#include "libpc.h"

PCLOSE(level)

	struct iorec		*level;
{
	register struct iorec	*next;

	next = _fchain.fchain;
	while(next != FILNIL && next->flev <= level) {
		next = PFCLOSE(next);
	}
	_fchain.fchain = next;
}
