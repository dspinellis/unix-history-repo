/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)Ovadvise.s	5.3 (Berkeley) %G%";
#endif not lint

#include "SYS.h"

#define	SYS_vadvise	72

SYSCALL(vadvise)
	ret
