/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)insque.s	5.1 (Berkeley) %G%";
#endif not lint

/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque, 0)
	insque	*4(ap), *8(ap)
	ret
