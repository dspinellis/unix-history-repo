/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)nargs.s	5.1 (Berkeley) %G%";
#endif not lint

/* C library -- nargs */

#include "DEFS.h"

ENTRY(nargs, 0)
	movzbl	*8(fp),r0	/* 8(fp) is old ap */
	ret
