/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwend.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

/*ARGSUSED*/
wwend(exit)
{
	if (tt.tt_checkpoint) {
		(void) alarm(0);
		wwdocheckpoint = 0;
	}
}
