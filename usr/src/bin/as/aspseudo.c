/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)aspseudo.c	5.1 (Berkeley) 4/30/85";
#endif not lint

#include <stdio.h>
#include "as.h"

#define	OP(name, eopcode, popcode, nargs, arg1, arg2, arg3, arg4, arg5, arg6) \
	{ \
		name, popcode, nargs, arg1, arg2, arg3, arg4, arg5, arg6, \
		(nargs == 0 ? INST0:INSTn), eopcode \
	}
#define	PSEUDO(name, type, tag) \
	{ \
		name, type, 0,   0, 0, 0, 0, 0, 0, \
		tag, CORE \
	}

readonly struct Instab instab[] = {
#include "instrs.as"
PSEUDO("\0\0\0\0\0\0\0\0\0\0", 0, 0)
};
