/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)remque.s	5.1 (Berkeley) %G%";
#endif not lint

/* remque(entry) */

#include "DEFS.h"

ENTRY(remque, 0)
	remque	*4(ap),r0
	ret
