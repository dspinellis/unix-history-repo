/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PCLOSE.c	1.7 (Berkeley) %G%";
#endif /* not lint */

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
		next = PFCLOSE(next, TRUE);
	}
	_fchain.fchain = next;
}
