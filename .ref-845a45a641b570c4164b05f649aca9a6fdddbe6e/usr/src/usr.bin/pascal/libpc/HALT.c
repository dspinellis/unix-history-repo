/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)HALT.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

HALT()
{
		PFLUSH();
		fputs("Call to procedure halt\n", stderr);
		PCEXIT(0);
}
